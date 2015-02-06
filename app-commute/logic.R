library(dplyr)
library(magrittr)
library(RSQLite)
library(ggmap)
library(scales)
library(RColorBrewer)
library(spam)
library(quantreg)
library(splines)

MungeCommuteDB <- function(src.commute, updateProgress = NULL) {
  
  if(is.function(updateProgress)) updateProgress('Quering the database')
  tbl.points <- tbl(src.commute, sql("
    SELECT
      src.*
      ,ref.duration_min as duration_minutes
      ,(ref.time - ref.duration_min/60) as time_start_hours
    FROM location as src
    INNER JOIN commute as ref on
    src.date = ref.date
    and src.time between (ref.time - ref.duration_min/60 - 1) and (ref.time + 1)
    ORDER BY src.date, src.time desc
    ")) %>%
    collect() %>%
    mutate(
      date = as.Date(date, '%m-%d-%Y')
      ,direction = factor(ifelse(time>12, 'Evening', 'Morning'), levels=c('Morning', 'Evening'))
      ,date_direction = factor(paste(date, as.character(direction), sep='_'))
    ) %>%
    select(-datetime_measure) %T>%
    str()
  
  work.long <- tbl.points %>%
    filter(direction == 'Morning') %>%
    group_by(date) %>%
    summarize(work.long = long[which.max(time)]) %$%
    mean(work.long, trim=0.1) %T>%
    print()
  
  tbl.trips <- tbl.points %>%
    mutate(
      time_start_hours = round(time_start_hours, 2)
      ,long.deviance = abs(long - work.long)
    ) %>%
    group_by(
      date
      ,direction
      ,time_start_hours
      ,duration_minutes
    ) %>%
    summarize(
      longitude_widest = long[which.max(long.deviance)]
    ) %>%
    ungroup() %T>%
    str()
  
  tbl.points %<>% left_join(
    tbl.trips %>% select(
      date
      ,direction
      ,longitude_widest
    )
  ) %T>% str()
  
  i.bbox <- make_bbox(long, lat, tbl.points, f = 0.1)
  
  if(is.function(updateProgress)) updateProgress('Quering Open Street Maps (SLOW)')
  OSMCondHandler <- function(cond) {
    print('Caught exception condition; loading cached map.')
    return(readRDS('cache.osm.RDS'))
  }
  
  i.osm <- tryCatch({
    stop('Stop bothering OSM during development.')
    i.osm <- get_openstreetmap(bbox = i.bbox, scale=1E5, messaging=TRUE)
    saveRDS(i.osm, 'cache.osm.RDS')
    i.osm
  }
  ,error = OSMCondHandler
  ,warning = OSMCondHandler
  )
  
  if(is.function(updateProgress)) updateProgress('Pre-calculating mapping assets', value.reduce.pct = 0.8)
  
  ggmap.base <- ggmap(
    i.osm
    ,darken = c(0.5, 'white')
  )
  
  n.tiles.wide <- 142L ## Limited by the amount of RAM that can be allocated
  tbl.tiles <- expand.grid(
    ## The sequence order needs to match with the population of the raster matrix later
    lat = seq(i.bbox['top'], i.bbox['bottom'], length.out = n.tiles.wide)
    ,lon = seq(i.bbox['left'], i.bbox['right'], length.out = n.tiles.wide)
  ) %>%
    as.tbl()
  
  return(list(
    tbl.points = tbl.points
    ,tbl.trips = tbl.trips
    ,i.bbox = i.bbox
    ,i.osm = i.osm
    ,ggmap.base = ggmap.base
    ,tbl.tiles = tbl.tiles
  ))
}

ApplyFilters <- function(
  i.tbl
  ,active_directions = levels(i.tbl$direction)
  ,active_date_range = range(i.tbl$date)
  ,active_departure_range = range(i.tbl$time_start_hours)
) {
  validate(need(
    diff(active_date_range) >= 0
    ,message=paste(
      'Date range is backwards:'
      ,active_date_range[1]
      ,'to'
      ,active_date_range[2]
    )
  ))
  
  i.tbl %>%
    filter(direction %in% active_directions) %>%
    filter(between(date, active_date_range[1], active_date_range[2])) %>%
    filter(between(time_start_hours, active_departure_range[1], active_departure_range[2]))
}

ColorRamp_DynamicAlpha <- function(
  i.color.norm
  ,i.alpha.norm = 1
  ,i.ramp.func = brewer.pal(11, 'RdYlBu') %>% rev() %>% colorRamp()
) {
  i.color.ramp <- i.ramp.func(i.color.norm)
  rgb(i.color.ramp[,1], i.color.ramp[,2], i.color.ramp[,3], i.alpha.norm*255, maxColorValue = 255L)
}

CreateHeatMap <- function(
  src.list
  ,active.points
  ,kernel_bandwidth_miles = 0.5
  ,kernel_function_power = 3
  ,alpha_saturation_limit = 0.95
  ,alpha_transform_power = 0.5
  ,duration_winsor_percent = 0.01
  ,path.trace.n.max = 5L
  ,updateProgress = NULL
) {
  #src.list <- i.components; active.points <- ApplyFilters(src.list$tbl.points) %>% arrange(date, time)
  #kernel_bandwidth_miles <- 0.5; kernel_function_power <- 3
  #alpha_saturation_limit <- 0.95; alpha_transform_power <- 0.5; duration_winsor_percent <- 0.05
  
  if(nrow(active.points) == 0) {return(src.list$ggmap.base)}
  
  if(is.function(updateProgress)) updateProgress('Calculating distances')
  mtx.dist.kde <- nearest.dist(
    x=active.points %>% select(long, lat) %>% as.matrix() ## Observed
    ,y=src.list$tbl.tiles %>% select(lon, lat) %>% as.matrix() ## Measured
    ,method='greatcircle'
    ,delta = kernel_bandwidth_miles * (360/(3963.34*2*pi)) ##Converts from miles to necessary ~delta
  )
  if(is.function(updateProgress)) updateProgress('Calculating weights')
  mtx.dist.kde@entries <- mtx.dist.kde@entries / (max(mtx.dist.kde@entries)*(1 + 1e-6))
  mtx.dist.kde@entries <- (1-mtx.dist.kde@entries^2)^kernel_function_power
  
  if(is.function(updateProgress)) updateProgress('Calculating tile values')
  i.tiles <- src.list$tbl.tiles %>% mutate(
    
    ## Compute the average duration for each tile using the kernel weighting from above
    denominator = colSums(mtx.dist.kde)
    ,numerator = as.vector(t(mtx.dist.kde) %*% matrix(active.points$duration, ncol = 1))
    ,duration.avg = numerator / denominator
    
    ## Compute the transparency `alpha` from the kernel weights above
    ,alpha.cap = pmin(denominator, pmax(quantile(denominator, alpha_saturation_limit), 0.001)) ## Home/Work areas will be over saturated, so cap them
    ,alpha.scale = (alpha.cap / max(alpha.cap))^alpha_transform_power ## Encourage more to be seen than is truly given weight
    
    ## Transform the average duration to a nice (0,1) scale to map into colors
    ,duration.avg.cap = pmax( ## Lightly cap for robustness properties
      pmin(
        duration.avg
        ,quantile(duration.avg, (1-duration_winsor_percent), na.rm = TRUE)
      )
      ,quantile(duration.avg, duration_winsor_percent, na.rm = TRUE)
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
  i.tiles$raster.color[!ind.empty] <- ColorRamp_DynamicAlpha(
    i.tiles$duration.avg.scale[!ind.empty]
    ,i.tiles$alpha.scale[!ind.empty]
  )
  raster.duration <- matrix(i.tiles$raster.color, nrow=sqrt(nrow(i.tiles)))
  
  if(n_distinct(active.points$date_direction) <= path.trace.n.max){
    if(is.function(updateProgress)) updateProgress('Adding paths')
    plt.heatmap.base <- src.list$ggmap.base +
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
    plt.heatmap.base <- src.list$ggmap.base
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
      ,colours = ColorRamp_DynamicAlpha(seq(0,1,by=0.05))
    )
  
  return(plt.heatmap)
}

ScatterLongitudeWidest <- function(active.trips) { ##active.trips <- i.components$tbl.trips
  
  plt.long <- ggplot(
    active.trips
    ,aes(
      x = longitude_widest
      ,y = duration_minutes
    )
  ) +
    geom_point(
      size = 5
      ,alpha = 0.5
      ,color = muted('blue')
    ) +
    stat_quantile(
      formula = as.formula(y~ns(x, 2))
      ,quantiles = seq(0.2, 0.8, 0.3)
      ,aes(color = ..quantile..)
    ) +
    scale_y_continuous('Commute Duration (Minutes)') +
    scale_x_continuous('Widest Longitude Observed During Commute') +
    scale_color_gradientn(
      'Quantile'
      ,colours =  c('orange', 'red')
      ,labels = percent_format()
    ) +
    theme_light()
  
  plt.long
  
}
