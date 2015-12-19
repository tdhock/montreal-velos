works_with_R("3.2.2",
             data.table="1.9.6",
             dplyr="0.4.3",
             "tdhock/ggplot2@a8b06ddb680acdcdbd927773b1011c562134e4d2",
             "tdhock/animint@3b1f84ec926ffbd765f0aa004596e43203750fd4")

load("velos.RData")
load("bike.paths.RData")
load("counter.locations.RData")
load("places.RData")

one.day <- 60 * 60 * 24

velos.dt <- data.table(velos)
velos.dt[, month.str := strftime(date, "%Y-%m")]
velos.dt[, month.text := strftime(date, "%B %Y")]
velos.dt[, month := factor(month.text, unique(month.text))]
velos.dt[, month.POSIXct := as.POSIXct(
  strptime(paste0(month.str, "-15"), "%Y-%m-%d"))]
counts.per.month <- velos.dt[, list(
  count=sum(count)
), by=.(location, month, month.POSIXct)][0 < count,]
months <- counts.per.month[, list(
  count=sum(count)
), by=.(month, month.POSIXct)]
months[, next.POSIXct := month.POSIXct + one.day * 30]
months[, month01.str := paste0(strftime(month.POSIXct, "%Y-%m"), "-01")]
months[, month01.POSIXct := as.POSIXct(strptime(month01.str, "%Y-%m-%d"))]
months[, next01.str := paste0(strftime(next.POSIXct, "%Y-%m"), "-01")]
months[, next01.POSIXct := as.POSIXct(strptime(next01.str, "%Y-%m-%d"))]
month.labels <- counts.per.month[, {
  .SD[count==max(count), ]
}, by=location]
  
places.dt <- data.table(places)

counter.locations[, lon := coord_X]
counter.locations[, lat := coord_Y]

velos <- velos %>%
  filter(!is.na(count))

loc.name.code <- c(
  "Berri1"="Berri",
  "Brebeuf"="Brébeuf",
  CSC="Côte-Sainte-Catherine",
  "Maisonneuve_1"="Maisonneuve 1",
  "Maisonneuve_2"="Maisonneuve 2",
  "Parc"="du Parc",
  PierDup="Pierre-Dupuy",
  "Rachel/Papineau"="Rachel",
  "Saint-Urbain"="Saint-Urbain",
  "Totem_Laurier"="Totem_Laurier")

counter.locations[, location := loc.name.code[nom_comptage]]
velo.counts <- table(velos$location)
setkey(counter.locations, location)
show.locations <- counter.locations[names(velo.counts)]
map.lim <- show.locations[, list(
  range.lat=range(lat),
  range.lon=range(lon)
)]
diff.vec <- sapply(map.lim, diff)
diff.mat <- c(-1, 1) * matrix(diff.vec, 2, 2, byrow=TRUE)
scale.mat <- as.matrix(map.lim) + diff.mat

location.colors <-
  c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", 
    "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")
names(location.colors) <- show.locations$location

setkey(show.locations, location)
setkey(counts.per.month, location)
counts.per.month.loc <- counts.per.month[show.locations,]

mtl.map <- ggplot()+
  theme_bw()+
  theme_animint(width=600)+
  theme(axis.line=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), axis.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())+
  coord_equal(xlim=map.lim$range.lon, ylim=map.lim$range.lat)+
  scale_color_manual(values=location.colors)+
  scale_x_continuous(limits=scale.mat[, "range.lon"])+
  scale_y_continuous(limits=scale.mat[, "range.lat"])+
  ##scale_size_manual("seasons", values=c(winter=2, "not winter"=0.5))+
  geom_path(aes(lon, lat,
                ##size=SAISONS4,
                tooltip=TYPE_VOIE,
                group=paste(feature.i, path.i)),
            color="grey",
            data=bike.paths)+
  geom_point(aes(lon, lat,
                 color=location,
                 size=count,
                 showSelected=month,
                 key=location,
                 clickSelects=location),
             data=counts.per.month.loc)+
  scale_size_animint()+
  ## geom_point(aes(lon, lat,
  ##                color=location,
  ##                clickSelects=location),
  ##            size=5,
  ##            data=show.locations)+
  guides(color="none")+
  geom_text(aes(lon, lat,
                label=location,
                clickSelects=location),
            data=show.locations)

##mtl.map+facet_wrap("month.POSIXct")

ggplot()+
  geom_line(aes(date, count),
            data=velos)+
  geom_point(aes(date, count),
             data=velos, pch=1, alpha=1/2)+
  theme_bw()+
  facet_wrap("location")+
  theme(panel.margin=grid::unit(0, "cm"))

dates <- velos %>%
  group_by(date) %>%
  summarise(locations=sum(!is.na(count))) %>%
  filter(locations > 0) %>%
  mutate(min.date=date-one.day/2,
         max.date=date+one.day/2)

location.labels <- velos %>%
  group_by(location) %>%
  filter(seq_along(count)==which.max(count))

location.ranges <- velos %>%
  group_by(location) %>%
  summarise(min=min(date),
            max=max(date))

LocSummary <- ggplot()+
  theme_bw()+
  theme_animint(width=350)+
  xlab("month")+
  geom_tallrect(aes(xmin=min.date, xmax=max.date,
                    clickSelects=date),
                data=dates, alpha=1/2)+
  scale_color_manual(values=location.colors)+
  geom_segment(aes(min, location,
                   xend=max, yend=location,
                   color=location,
                   clickSelects=location),
               data=location.ranges, alpha=3/4, size=10)
print(LocSummary)

MonthSummary <- ggplot()+
  theme_bw()+
  theme_animint(width=350)+
  xlab("date")+
  scale_color_manual(values=location.colors)+
  guides(color="none")+
  geom_tallrect(aes(xmin=month01.POSIXct, xmax=next01.POSIXct,
                    clickSelects=month),
                data=months, alpha=1/2)+
  geom_segment(aes(min, location,
                   xend=max, yend=location,
                   color=location,
                   clickSelects=location),
               data=location.ranges, alpha=3/4, size=10)
print(MonthSummary)

TimeSeries <- ggplot()+
  theme_bw()+
  theme_animint(width=1000)+
  geom_tallrect(aes(xmin=date-one.day/2, xmax=date+one.day/2,
                    clickSelects=date),
                data=dates, alpha=1/2)+
  geom_line(aes(date, count, group=location,
                showSelected=location,
                clickSelects=location),
            data=velos)+
  scale_color_manual(values=location.colors)+
  geom_point(aes(date, count, color=location,
                 showSelected=location,
                 clickSelects=location),
             data=velos)+
  geom_text(aes(date, count+200, color=location, label=location,
                showSelected=location,
                clickSelects=location),
            data=location.labels)
print(TimeSeries)

MonthSeries <- ggplot()+
  guides(color="none")+
  theme_bw()+
  theme_animint(width=1000)+
  geom_tallrect(aes(xmin=month01.POSIXct, xmax=next01.POSIXct,
                    clickSelects=month),
                data=months, alpha=1/2)+
  geom_line(aes(month.POSIXct, count, group=location,
                showSelected=location,
                clickSelects=location),
            data=counts.per.month)+
  scale_color_manual(values=location.colors)+
  xlab("month")+
  geom_point(aes(month.POSIXct, count, color=location,
                 showSelected=location,
                 clickSelects=location),
             data=counts.per.month)+
  geom_text(aes(month.POSIXct, count+2000, color=location, label=location,
                showSelected=location,
                clickSelects=location),
            data=month.labels)
print(MonthSeries)

bars <- ggplot()+
  geom_bar(aes(location, count, fill=location,
               key=location,
               clickSelects=location,
               showSelected=date),
           data=velos, stat="identity", position="identity",
           chunk_vars=character())+
  geom_text(aes("Totem_Laurier", 7500,
                label=strftime(date, "%A %d %B %Y"),
                showSelected=date),
            data=dates)+
  coord_flip()
print(bars)

viz <-
  list(##bars=bars+guides(fill="none"),
    ##TimeSeries=TimeSeries+guides(color="none"),
    MonthSeries=MonthSeries,
    ##summary=LocSummary+guides(color="none"),
    summary=MonthSummary,
    map=mtl.map,
    selector.types=list(location="multiple"),
    time=list(variable="month", ms=2000),
    duration=list(month=2000),
    first=list(location=c("Berri", "Rachel")),
    title="Montreal cyclists, 2009-2013")

animint2dir(viz, "figure-timeseries")

##animint2gist(viz)
