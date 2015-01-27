library(dplyr)
library(magrittr)
library(ggmap)
library(scales)
library(RColorBrewer)
library(spam)

dir.data <- paste0(Sys.getenv('UserProfile'),'/Google Drive/Skunkworks_OnePlus/') %T>% print()
commute.src <- src_sqlite(paste0(dir.data,'commute.sqlite')) %T>% print()

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
  ) %T>%
  print()

i.bbox <- make_bbox(long, lat, tbl.commute, f = 0.1) %T>% print()
i.osm <- get_openstreetmap(bbox = i.bbox, scale=1E5, messaging=TRUE)
if(!exists('i.osm')){ ## OSM web API will often deny queries under load, so cache some results
  i.osm <- loadRDS('cache.osm.RDS')
} else {
  saveRDS(i.osm, 'cache.osm.RDS')
}

n.tiles.wide <- 420L
tbl.tiles <- expand.grid(
  ## The sequence order needs to match with the population of the raster matrix below
  lat = seq(i.bbox['top'], i.bbox['bottom'], length.out = n.tiles.wide)
  ,lon = seq(i.bbox['left'], i.bbox['right'], length.out = n.tiles.wide)
) %>%
  as.tbl()

MyRamp <- colorRamp(rev(brewer.pal(11, 'RdYlBu'))) ## Traverse Red<>Yellow<>Blue
MyRampWrap <- function(i.color.norm, i.alpha.norm=1, i.ramp.func = MyRamp) {
  i.color.ramp <- i.ramp.func(i.color.norm)
  rgb(i.color.ramp[,1], i.color.ramp[,2], i.color.ramp[,3], i.alpha.norm*255, maxColorValue = 255L)
}

shinyServer(function(input, output) {
  output$heatmap <- renderPlot({
    mtx.dist.kde <- nearest.dist(
      x=tbl.commute %>% select(long, lat) %>% as.matrix() ## Observed
      ,y=tbl.tiles %>% select(lon, lat) %>% as.matrix() ## Measured
      ,method='greatcircle'
      ,delta = input$kernel.bandwidth.miles * (360/(3963.34*2*pi)) ##Converts from miles to necessary ~delta
    )
    mtx.dist.kde@entries <- mtx.dist.kde@entries / (max(mtx.dist.kde@entries)*(1 + 1e-6))
    mtx.dist.kde@entries <- (1-mtx.dist.kde@entries^2)^3
    
    i.tiles <- tbl.tiles %>% mutate(
      
      ## Compute the average duration for each tile using the kernel weighting from above
      denominator = colSums(mtx.dist.kde)
      ,numerator = as.vector(t(mtx.dist.kde) %*% matrix(tbl.commute$duration, ncol = 1))
      ,duration.avg = numerator / denominator
      
      ## Compute the transparency `alpha` from the kernel weights above
      ,alpha.cap = pmin(denominator, quantile(denominator, 0.95)) ## Home/Work areas will be over saturated, so cap them
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
    ind.empty <- is.na(i.tiles$duration.avg.scale) %T>% {mean(.) %>% print()}
    i.tiles$raster.color[!ind.empty] <- MyRampWrap(
      i.tiles$duration.avg.scale[!ind.empty]
      ,i.tiles$alpha.scale[!ind.empty]
    )
    raster.duration <- matrix(i.tiles$raster.color, nrow=n.tiles.wide)
    
    plt.heat <- ggmap(
      i.osm
      ,darken = c(0.5, 'white')
    ) + 
      inset_raster(
        raster.duration
        ,xmin = i.bbox['left'], xmax=i.bbox['right']
        ,ymin=i.bbox['bottom'],ymax=i.bbox['top']
      ) +
      scale_colour_gradientn(
        'Commute Time\n(Minutes)'
        ,guide = "colorbar"
        ,limits = range(i.tiles$duration.avg.cap[!ind.empty])
        ,colours=MyRampWrap(seq(0,1,by=0.05))
      )
    plt.heat
    
  })
})