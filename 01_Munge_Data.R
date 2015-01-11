#' # Simple exploration of commuting data
library(dplyr)
library(magrittr)
library(ggmap)

print(dir.data <- paste0(Sys.getenv('UserProfile'),'/Google Drive/Skunkworks_OnePlus/'))
print(commute.src <- src_sqlite(paste0(dir.data,'commute.sqlite')))
glimpse(tbl.location <- tbl(commute.src, 'location'))
print(i.bbox <- make_bbox(long, lat, tbl.location %>% collect(), f=0.2))
print(i.bbox <- i.bbox * c(0.999, 1, 1.001, 1))

i.osm <- get_openstreetmap(bbox = i.bbox, scale=1E5, messaging=TRUE)
ggmap(i.osm) + 
  geom_point(
    data=tbl.location %>%
      collect()
    ,aes(x=long,y=lat)
    ,size=9
    ,color='red'
    )

i.google <- get_googlemap(
  tbl.location %>% collect() %>% summarize(lon=mean(range(long)),lat=mean(range(lat))) %$% c(lon,lat)
  ,zoom=11)
ggmap(i.google) + 
  geom_point(
    data=tbl.location %>%
      collect() %>%
      mutate(
        fuzzy_acc = factor(ifelse(accuracy<420,'Good','Bad'))
        ,direction = factor(ifelse(time>12,'Evening','Morning'))
        ,date_direction = factor(paste0(date,as.character(direction)))
        )
    ,aes(
      x=long
      ,y=lat
      ,color=date_direction
      )
    ,size=5
    #,color='orange'
  ) + scale_color_brewer(palette = 'Dark2')
