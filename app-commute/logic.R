library(dplyr)
library(magrittr)
library(RSQLite)
library(ggmap)
library(scales)
library(RColorBrewer)
library(spam)

GenerateComponents <- function(commute.src) {
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
      direction = factor(ifelse(time>12, 'Evening', 'Morning'))
      ,date_direction = factor(paste0(date, as.character(direction)))
    )
  
  i.bbox <- make_bbox(long, lat, tbl.commute, f = 0.1)
  
  try(i.osm <- get_openstreetmap(bbox = i.bbox, scale=1E5, messaging=TRUE))
  if(!exists('i.osm')){ ## OSM web API will often deny queries under load, so cache some results
    i.osm <- readRDS('cache.osm.RDS')
  } else {
    saveRDS(i.osm, 'cache.osm.RDS')
  }
  
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
    ,tbl.tiles = tbl.tiles
    ,ColorRampFull = ColorRampFull
  ))
  
}

CreateHeatMap <- function(src.list, kernel.bandwidth.miles) {#src.list <- i.components; kernel.bandwidth.miles <- 0.5
  
  mtx.dist.kde <- nearest.dist(
    x=src.list$tbl.commute %>% select(long, lat) %>% as.matrix() ## Observed
    ,y=src.list$tbl.tiles %>% select(lon, lat) %>% as.matrix() ## Measured
    ,method='greatcircle'
    ,delta = kernel.bandwidth.miles * (360/(3963.34*2*pi)) ##Converts from miles to necessary ~delta
  )
  mtx.dist.kde@entries <- mtx.dist.kde@entries / (max(mtx.dist.kde@entries)*(1 + 1e-6))
  mtx.dist.kde@entries <- (1-mtx.dist.kde@entries^2)^3
  
  i.tiles <- src.list$tbl.tiles %>% mutate(
    
    ## Compute the average duration for each tile using the kernel weighting from above
    denominator = colSums(mtx.dist.kde)
    ,numerator = as.vector(t(mtx.dist.kde) %*% matrix(src.list$tbl.commute$duration, ncol = 1))
    ,duration.avg = numerator / denominator
    
    ## Compute the transparency `alpha` from the kernel weights above
    ,alpha.cap = pmin(denominator, pmax(quantile(denominator, 0.95), 0.001)) ## Home/Work areas will be over saturated, so cap them
    ,alpha.scale = sqrt(alpha.cap / max(alpha.cap)) ## Encourage more to be seen than is truly given weight
    
    ## Transform the average duration to a nice (0,1) scale to map into colors
    ,duration.avg.cap = pmax( ## Lightly cap for robustness properties
      pmin(
        duration.avg
        ,quantile(duration.avg,0.99,na.rm = TRUE)
      )
      ,quantile(duration.avg,0.01,na.rm = TRUE)
    )
    ,duration.avg.scale = ifelse(
      is.na(duration.avg.cap)
      ,NA
      ,(duration.avg.cap - min(duration.avg.cap, na.rm=TRUE))
      / (max(duration.avg.cap, na.rm=TRUE) - min(duration.avg.cap, na.rm=TRUE))
    )
  )
  
  i.tiles$raster.color <- rgb(0,0,0,0)
  ind.empty <- is.na(i.tiles$duration.avg.scale) ## %T>% {mean(.) %>% print()}
  i.tiles$raster.color[!ind.empty] <- src.list$ColorRampFull(
    i.tiles$duration.avg.scale[!ind.empty]
    ,i.tiles$alpha.scale[!ind.empty]
  )
  raster.duration <- matrix(i.tiles$raster.color, nrow=sqrt(nrow(i.tiles)))
  
  plt.heatmap <- ggmap(
    src.list$i.osm
    ,darken = c(0.5, 'white')
  ) + 
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
