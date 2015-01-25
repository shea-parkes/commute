#' # Simple exploration of commuting data
library(dplyr)
library(magrittr)
library(ggmap)
library(scales)
library(RColorBrewer)
library(spam)

dir.data <- paste0(Sys.getenv('UserProfile'),'/Google Drive/Skunkworks_OnePlus/') %T>% print()
commute.src <- src_sqlite(paste0(dir.data,'commute.sqlite')) %T>% print()

tbl.commute <- tbl(commute.src, sql("
  SELECT src.*,ref.duration_min
  FROM location as src
  INNER JOIN commute as ref on
    src.date = ref.date
    and src.time between (ref.time - ref.duration_min/60 - 1) and (ref.time + 1)
  ORDER BY src.date, src.time desc
")) %>%
  collect() %T>%
  print()

i.bbox <- make_bbox(long, lat, tbl.commute) %T>% print()
i.osm <- get_openstreetmap(bbox = i.bbox, scale=1E5, messaging=TRUE)
if(is.null(i.osm)){
  i.osm <- get_googlemap(
    tbl.commute %>%
      summarize(
        lon=mean(range(long))
        ,lat=mean(range(lat))
      ) %$%
    c(lon,lat)
  ,zoom=11
  )
}

tbl.prep <- tbl.commute %>%
  mutate(
    fuzzy_acc = factor(ifelse(accuracy<420,'Good','Bad'))
    ,direction = factor(ifelse(time>12,'Evening','Morning'))
    ,date_direction = factor(paste0(date,as.character(direction)))
  ) %T>%
  glimpse()

plt.commute <- ggmap(
  i.osm
  ,base_layer=ggplot(
    data=tbl.prep
    ,aes(
      x=long
      ,y=lat
      ,color=duration_min
      )
    )
  ,darken = c(0.4, 'white')
  ) + 
  geom_point(
    aes(alpha=1/sqrt(accuracy))
    ,size=5
    ) +
  geom_path(
    aes(group=date_direction)
    ,size=1
    ,alpha=0.8
    ) +
  scale_size_continuous()+
  scale_alpha_continuous(
    guide='none'
    ,range=c(0.5,0.95)
    )+
  scale_color_gradientn(
    "Duration\n(Minutes)"
    ,colours=rev(brewer.pal(11, 'RdBu'))[-(5:7)]
    )
plt.commute
ggsave(
  paste0(dir.data,'commute.png')
  ,plt.commute
  ,width=8
  ,height=8
  )

## Approximate miles conversions from here: http://geography.about.com/library/faq/blqzdistancedegree.htm
## Some useful Raster info here: http://stackoverflow.com/questions/25847188/geographical-heat-map-of-a-custom-property-in-r-with-ggmap

#' Make the grid of points to measure
n.tiles.wide <- 420L
i.tiles <- expand.grid(
  ## The column order should be (lon,lat) for nearest.dist()
  ## The sequence order inside the columns are important for making the raster matrix
  lon = seq(i.bbox['left'], i.bbox['right'], length.out = n.tiles.wide)
  ,lat = seq(i.bbox['top'], i.bbox['bottom'], length.out = n.tiles.wide)
  )

#' Calculate distances between measurement points and observed points
threshold.miles <- 1.5 ## Distances above this do not get captured
i.dist <- nearest.dist(
  x=as.matrix(tbl.prep %>% select(long, lat)) ## Observed
  ,y=as.matrix(i.tiles[,c('lon','lat')]) ## Measured
  ,method='greatcircle'
  ,delta = threshold.miles * (360/(3963.34*2*pi)) ##Converts from miles to necessary ~delta
  )
hist(i.dist@entries, breaks='FD')

#' Calculate weights from distances
max(i.dist@entries) ## Should be almost equal to threshold.miles (with floating point error)
i.dist.wgt <- i.dist
i.dist.wgt@entries <- i.dist.wgt@entries / (max(i.dist.wgt@entries)*(1 + 1e-6)) ## Normalize them to (0,1) prior to applying kernel
## Kernel work explained here: http://en.wikipedia.org/wiki/Kernel_(statistics)
#i.dist.wgt@entries <- exp(-1*(i.dist.wgt@entries^2)/2) ##Gaussian Kernel (improper support)
#i.dist.wgt@entries <- (1-i.dist.wgt@entries^2) ## Epanechnikov
#i.dist.wgt@entries <- (1-i.dist.wgt@entries^2)^2 ## Quartic (biweight) Kernel
i.dist.wgt@entries <- (1-i.dist.wgt@entries^2)^3 ## Triweight
#i.dist.wgt@entries <- cos(i.dist.wgt@entries*pi/2) ## Cosine
hist(i.dist.wgt@entries, breaks='FD')

#' Bring back information to the measurement points
i.tiles$denom <- colSums(i.dist.wgt)
i.tiles$num <- as.vector(t(i.dist.wgt) %*% matrix(tbl.prep$duration_min, ncol = 1))
hist(i.tiles$denom, breaks=42)
i.tiles %<>% mutate(
    duration_min_avg = num / denom
    ,alpha = pmin(denom, quantile(denom,0.95))
    ,alpha.scale = (alpha / max(alpha))^0.5
    ,duration_min_avg.cap = pmax(
      pmin(
        duration_min_avg
        ,quantile(duration_min_avg,0.99,na.rm = TRUE)
        )
      ,quantile(duration_min_avg,0.01,na.rm = TRUE)
    )
    ,duration_min_avg.scale = ifelse(
      is.na(duration_min_avg.cap)
      ,NA
      ,(duration_min_avg.cap - min(duration_min_avg.cap, na.rm=TRUE))
        / (max(duration_min_avg.cap, na.rm=TRUE) - min(duration_min_avg.cap, na.rm=TRUE))
      )
    )
hist(i.tiles$alpha.scale, breaks=42)
hist(i.tiles$duration_min_avg.scale, breaks=42)
summary(i.tiles)

#' Calculate an interesting color to help build a raster object
#MyRamp <- colorRamp(rev(brewer.pal(11, 'RdBu')[-(5:7)])) ## A more direct Red<>Blue
MyRamp <- colorRamp(rev(brewer.pal(11, 'RdYlBu'))) ## Traverse Red<>Yellow<>Blue
MyRampWrap <- function(i.color.norm, i.alpha.norm=1, i.ramp.func = MyRamp) {
  i.color.ramp <- i.ramp.func(i.color.norm)
  rgb(i.color.ramp[,1], i.color.ramp[,2], i.color.ramp[,3], i.alpha.norm*255, maxColorValue = 255L)
  }

i.tiles$raster.color <- rgb(0,0,0,0)
ind.empty <- is.na(i.tiles$duration_min_avg.scale) %T>% {mean(.) %>% print()}
i.tiles$raster.color[!ind.empty] <- MyRampWrap(
  i.tiles$duration_min_avg.scale[!ind.empty]
  ,i.tiles$alpha.scale[!ind.empty]
  )
my.raster <- matrix(i.tiles$raster.color, nrow=n.tiles.wide, byrow=TRUE)

#' Plot the resulting heat map
plt.heat <- ggmap(
  i.osm
  ,darken = c(0.5, 'white')
  ) + 
  inset_raster(
    my.raster
    ,xmin = i.bbox['left'], xmax=i.bbox['right']
    ,ymin=i.bbox['bottom'],ymax=i.bbox['top']
    ) +
scale_colour_gradientn(
  'Commute Time\n(Minutes)'
  ,guide = "colorbar"
  ,limits = range(i.tiles$duration_min_avg.cap[!ind.empty])
  ,colours=MyRampWrap(seq(0,1,by=0.05))
  )
plt.heat
ggsave(
  paste0(dir.data,'commute_heat.png')
  ,plt.heat
  ,width=12
  ,height=12
  )
