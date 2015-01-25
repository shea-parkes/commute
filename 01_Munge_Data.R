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

n.tiles.wide <- 420L
i.tiles <- expand.grid(
  lon = seq(i.bbox['left'], i.bbox['right'], length.out = n.tiles.wide)
  ,lat = seq(i.bbox['bottom'], i.bbox['top'], length.out = n.tiles.wide)
  )
i.dist <- nearest.dist(
  x=as.matrix(tbl.prep %>% select(long, lat))
  ,y=as.matrix(i.tiles)
  ,method='greatcircle'
  ,delta = 2.5 * (360/(3963.34*2*pi)) ##Converts from miles to necessary ~delta
  )
str(i.dist)
i.dist.wgt <- i.dist
hist(i.dist.wgt@entries, breaks='FD')
i.dist.wgt@entries <- 1/(exp(i.dist.wgt@entries^2))
hist(i.dist.wgt@entries, breaks='FD')

i.tiles$denom <- colSums(i.dist.wgt)
i.tiles$num <- as.vector(t(i.dist.wgt) %*% matrix(tbl.prep$duration_min, ncol = 1))
hist(i.tiles$denom, breaks=42)
i.tiles %<>% mutate(
    duration_min_avg = num / denom
    ,alpha = pmin(denom, quantile(denom,0.95))
    ,alpha.scale = alpha / max(alpha)
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
    #,color = MyRampWrap(duration_min_avg.scale, alpha.scale)
    )
hist(i.tiles$alpha.scale, breaks=42)
hist(i.tiles$duration_min_avg.scale, breaks=42)
summary(i.tiles)


MyRamp <- colorRamp(brewer.pal(11, 'RdBu')[-(5:7)])
# seq.sample <- seq(0,1,by = 0.01)
# seq.sample.color.ramp <- MyRamp(seq.sample)
# seq.sample.color.vector <- rgb(seq.sample.color.ramp[,1],seq.sample.color.ramp[,2],seq.sample.color.ramp[,3],alpha=255,maxColorValue=255)
# plot(x=seq.sample,y=rep(1,length(seq.sample)),col=seq.sample.color.vector,pch=16)
MyRampWrap <- function(i.color.norm, i.alpha.norm=1, i.ramp.func = MyRamp) {
  i.color.ramp <- i.ramp.func(i.color.norm)
  rgb(i.color.ramp[,1], i.color.ramp[,2], i.color.ramp[,3], i.alpha.norm*255, maxColorValue = 255L)
}
i.tiles$raster.color <- rgb(0,0,0,0)
i.tiles$raster.color[!is.na(i.tiles$duration_min_avg.scale)] <- MyRampWrap(
  i.tiles$duration_min_avg.scale[!is.na(i.tiles$duration_min_avg.scale)]
  ,sqrt(i.tiles$alpha.scale[!is.na(i.tiles$duration_min_avg.scale)])
  )
i.tiles$raster.color %>% table() %>% sort() %>% tail(20)

my.raster <- matrix(i.tiles$raster.color, nrow=n.tiles.wide,byrow=TRUE)
str(my.raster)

## Some useful Raster info here: http://stackoverflow.com/questions/25847188/geographical-heat-map-of-a-custom-property-in-r-with-ggmap

plt.heat <- ggmap(
  i.osm
  ,darken = c(0.3, 'white')
) + 
  inset_raster(my.raster, xmin = i.bbox['left'], xmax=i.bbox['right'], ymin=i.bbox['bottom'],ymax=i.bbox['top'])
  
plt.heat

