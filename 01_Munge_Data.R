#' # Simple exploration of commuting data
library(dplyr)
library(magrittr)
library(ggmap)
library(scales)
library(RColorBrewer)

print(dir.data <- paste0(Sys.getenv('UserProfile'),'/Google Drive/Skunkworks_OnePlus/'))
print(commute.src <- src_sqlite(paste0(dir.data,'commute.sqlite')))

print(tbl.commute <- tbl(commute.src, sql("
  SELECT src.*,ref.duration_min
  FROM location as src
  INNER JOIN commute as ref on
    src.date = ref.date
    and src.time between (ref.time - ref.duration_min/60 - 1) and (ref.time + 1)
  ORDER BY src.date, src.time desc
")) %>% collect())

print(i.bbox <- make_bbox(long, lat, tbl.commute, f=0.2))
i.osm <- get_openstreetmap(bbox = i.bbox, scale=1E5, messaging=TRUE)
#i.osm <- get_googlemap(tbl.commute %>% summarize(lon=mean(range(long)),lat=mean(range(lat))) %$% c(lon,lat),zoom=11)

glimpse(tbl.prep <- tbl.commute %>%
  mutate(
    fuzzy_acc = factor(ifelse(accuracy<420,'Good','Bad'))
    ,direction = factor(ifelse(time>12,'Evening','Morning'))
    ,date_direction = factor(paste0(date,as.character(direction)))
  ))


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