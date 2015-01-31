library(dplyr)
library(magrittr)
library(RSQLite)
library(ggmap)
library(scales)
library(RColorBrewer)
library(spam)

GenerateComponents <- function(commute.src, updateProgress = NULL) {
  
  if(is.function(updateProgress)) updateProgress('Quering the database')
  tbl.commute <- tbl(commute.src, sql("
    SELECT src.*,ref.duration_min as duration
    FROM location as src
    INNER JOIN commute as ref on
    src.date = ref.date
    and src.time between (ref.time - ref.duration_min/60 - 1) and (ref.time + 1)
    ORDER BY src.date, src.time desc
    ")) %>%
    collect() %>%
    mutate(
      date.parse = as.Date(date, '%m-%d-%Y')
      ,direction = factor(ifelse(time>12, 'Evening', 'Morning'), levels=c('Morning', 'Evening'))
      ,date_direction = factor(paste(date.parse, as.character(direction), sep='_'))
    ) %T>% str()
  
  i.bbox <- make_bbox(long, lat, tbl.commute, f = 0.1)
  
  if(is.function(updateProgress)) updateProgress('Quering Open Street Maps (SLOW)')
  OSMCondHandler <- function(cond) {
    print('Caught exception condition; loading cached map.')
    return(readRDS('cache.osm.RDS'))
  }
  
  i.osm <- tryCatch({
    i.osm <- get_openstreetmap(bbox = i.bbox, scale=1E5, messaging=TRUE)
    saveRDS(i.osm, 'cache.osm.RDS')
    i.osm
  }
  ,error = OSMCondHandler
  ,warning = OSMCondHandler
  )
  
  if(is.function(updateProgress)) updateProgress('Pre-calculating mapping assets', value.reduce.pct = 0.8)
  
  base.ggmap <- ggmap(
    i.osm
    ,darken = c(0.5, 'white')
  )
  
  n.tiles.wide <- 142L ## Limited by the amount of RAM that can be allocated
  tbl.tiles <- expand.grid(
    ## The sequence order needs to match with the population of the raster matrix below
    lat = seq(i.bbox['top'], i.bbox['bottom'], length.out = n.tiles.wide)
    ,lon = seq(i.bbox['left'], i.bbox['right'], length.out = n.tiles.wide)
  ) %>%
    as.tbl()
  
  
  ColorRampSimple <- colorRamp(rev(brewer.pal(11, 'RdYlBu'))) ## Traverse Red<>Yellow<>Blue
  ColorRampFull <- function(i.color.norm, i.alpha.norm=1, i.ramp.func = ColorRampSimple) {
    i.color.ramp <- i.ramp.func(i.color.norm)
    rgb(i.color.ramp[,1], i.color.ramp[,2], i.color.ramp[,3], i.alpha.norm*255, maxColorValue = 255L)
  }
  
  return(list(
    tbl.commute = tbl.commute
    ,i.bbox = i.bbox
    ,i.osm = i.osm
    ,base.ggmap = base.ggmap
    ,tbl.tiles = tbl.tiles
    ,ColorRampFull = ColorRampFull
  ))
}

ApplyFilters <- function(
  src.list
  ,active.directions = levels(src.list$tbl.commute$direction)
  ,active.date.range = range(src.list$tbl.commute$date.parse)
) {
  src.list$tbl.commute %>%
    filter(direction %in% active.directions) %>%
    filter(between(date.parse, active.date.range[1], active.date.range[2])) %>%
    arrange(date, time)
}

CreateHeatMap <- function(
  src.list
  ,active.points
  ,kernel.bandwidth.miles = 0.5
  ,kernel.function.power = 3
  ,alpha.saturation.limit = 0.95
  ,alpha.transform.power = 0.5
  ,duration.winsor.percent = 0.01
  ,path.trace.n.max = 5L
  ,updateProgress = NULL
  ) {
  #src.list <- i.components; active.points <- ApplyFilters(src.list)
  #kernel.bandwidth.miles <- 0.5; kernel.function.power <- 3
  #alpha.saturation.limit <- 0.95; alpha.transform.power <- 0.5; duration.winsor.percent <- 0.05
  
  if(nrow(active.points) == 0) {return(src.list$base.ggmap)}
  
  if(is.function(updateProgress)) updateProgress('Calculating distances')
  mtx.dist.kde <- nearest.dist(
    x=active.points %>% select(long, lat) %>% as.matrix() ## Observed
    ,y=src.list$tbl.tiles %>% select(lon, lat) %>% as.matrix() ## Measured
    ,method='greatcircle'
    ,delta = kernel.bandwidth.miles * (360/(3963.34*2*pi)) ##Converts from miles to necessary ~delta
  )
  if(is.function(updateProgress)) updateProgress('Calculating weights')
  mtx.dist.kde@entries <- mtx.dist.kde@entries / (max(mtx.dist.kde@entries)*(1 + 1e-6))
  mtx.dist.kde@entries <- (1-mtx.dist.kde@entries^2)^kernel.function.power
  
  if(is.function(updateProgress)) updateProgress('Calculating tile values')
  i.tiles <- src.list$tbl.tiles %>% mutate(
    
    ## Compute the average duration for each tile using the kernel weighting from above
    denominator = colSums(mtx.dist.kde)
    ,numerator = as.vector(t(mtx.dist.kde) %*% matrix(active.points$duration, ncol = 1))
    ,duration.avg = numerator / denominator
    
    ## Compute the transparency `alpha` from the kernel weights above
    ,alpha.cap = pmin(denominator, pmax(quantile(denominator, alpha.saturation.limit), 0.001)) ## Home/Work areas will be over saturated, so cap them
    ,alpha.scale = (alpha.cap / max(alpha.cap))^alpha.transform.power ## Encourage more to be seen than is truly given weight
    
    ## Transform the average duration to a nice (0,1) scale to map into colors
    ,duration.avg.cap = pmax( ## Lightly cap for robustness properties
      pmin(
        duration.avg
        ,quantile(duration.avg, (1-duration.winsor.percent), na.rm = TRUE)
      )
      ,quantile(duration.avg, duration.winsor.percent, na.rm = TRUE)
    )
    ,duration.avg.scale = ifelse(
      is.na(duration.avg.cap)
      ,NA
      ,(duration.avg.cap - min(duration.avg.cap, na.rm=TRUE))
      / (max(duration.avg.cap, na.rm=TRUE) - min(duration.avg.cap, na.rm=TRUE))
    )
  )
  
  if(is.function(updateProgress)) updateProgress('Calculating tile colors')
  i.tiles$raster.color <- rgb(0,0,0,0)
  ind.empty <- is.na(i.tiles$duration.avg.scale) ## %T>% {mean(.) %>% print()}
  i.tiles$raster.color[!ind.empty] <- src.list$ColorRampFull(
    i.tiles$duration.avg.scale[!ind.empty]
    ,i.tiles$alpha.scale[!ind.empty]
  )
  raster.duration <- matrix(i.tiles$raster.color, nrow=sqrt(nrow(i.tiles)))
  
  if(n_distinct(active.points$date_direction) <= path.trace.n.max){
    if(is.function(updateProgress)) updateProgress('Adding paths')
    plt.heatmap.base <- src.list$base.ggmap +
      geom_path(
        data=active.points
        ,aes(
          x=long
          ,y=lat
          ,group=date_direction
          )
        ,size=1
        ,alpha=0.6
        ,lty=2
      )
  } else {
    plt.heatmap.base <- src.list$base.ggmap
  }
  
  if(is.function(updateProgress)) updateProgress('Compiling plot')
  plt.heatmap <- plt.heatmap.base + 
    inset_raster(
      raster.duration
      ,xmin = src.list$i.bbox['left'], xmax = src.list$i.bbox['right']
      ,ymin = src.list$i.bbox['bottom'], ymax = src.list$i.bbox['top']
    ) +
    scale_colour_gradientn(
      'Commute Time\n(Minutes)'
      ,guide = "colorbar"
      ,limits = range(i.tiles$duration.avg.cap[!ind.empty])
      ,colours=src.list$ColorRampFull(seq(0,1,by=0.05))
    )
  
  return(plt.heatmap)
}
