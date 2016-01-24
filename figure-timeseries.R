works_with_R("3.2.2",
             data.table="1.9.6",
             dplyr="0.4.3",
             "tdhock/ggplot2@a8b06ddb680acdcdbd927773b1011c562134e4d2",
             "tdhock/animint@3b1f84ec926ffbd765f0aa004596e43203750fd4")

load("velos.RData")
load("bike.paths.RData")
load("counter.locations.RData")
load("accidents.RData")

one.day <- 60 * 60 * 24

accidents.dt <- data.table(accidents)
accidents.dt[, date.POSIXct := as.POSIXct(strptime(date.str, "%Y-%m-%d"))]
accidents.dt[, month.str := strftime(date.POSIXct, "%Y-%m")]

velos.dt <- data.table(velos)
velos.dt[, month.str := strftime(date, "%Y-%m")]

uniq.month.vec <- unique(c(
  accidents.dt$month.str,
  velos.dt[!(count==0 | is.na(count)), month.str]))
months <- data.table(month.str=uniq.month.vec)
months[, month01.str := paste0(month.str, "-01")]
months[, month01.POSIXct := as.POSIXct(strptime(month01.str, "%Y-%m-%d"))]
months[, next.POSIXct := month01.POSIXct + one.day * 31]
months[, next01.str := paste0(strftime(next.POSIXct, "%Y-%m"), "-01")]
months[, next01.POSIXct := as.POSIXct(strptime(next01.str, "%Y-%m-%d"))]
months[, month := strftime(month01.POSIXct, "%B %Y")]

month.levs <- months[order(month01.POSIXct), month]

accidents.dt[, month.text := strftime(date.POSIXct, "%B %Y")]
accidents.dt[, month := factor(month.text, month.levs)]
accidents.dt[, month.POSIXct := as.POSIXct(
  strptime(paste0(month.str, "-15"), "%Y-%m-%d"))]
stopifnot(!is.na(accidents.dt$month.POSIXct))
accidents.per.month <- accidents.dt[, list(
  deaths=sum(deaths),
  people.severely.injured=sum(people.severely.injured),
  people.slightly.injured=sum(people.slightly.injured)
), by=.(month, month.str, month.text, month.POSIXct)]
accidents.per.month[, next.POSIXct := month.POSIXct + one.day * 30]
accidents.per.month[, month01.str := paste0(strftime(month.POSIXct, "%Y-%m"), "-01")]
accidents.per.month[, month01.POSIXct := as.POSIXct(strptime(month01.str, "%Y-%m-%d"))]
accidents.per.month[, next01.str := paste0(strftime(next.POSIXct, "%Y-%m"), "-01")]
accidents.per.month[, next01.POSIXct := as.POSIXct(strptime(next01.str, "%Y-%m-%d"))]

severity.vec <- c(
  "people slightly injured",
  "people severely injured",
  "deaths")
accidents.by.severity <- list()
for(severity in severity.vec){
  severity.col <- gsub(" ", ".", severity)
  people <- accidents.per.month[[severity.col]]
  accidents.by.severity[[severity]] <-
    data.table(accidents.per.month,
               severity=factor(severity, severity.vec),
               people)
}
accidents.tall <- do.call(rbind, accidents.by.severity)

velos.dt[, month.text := strftime(date, "%B %Y")]
velos.dt[, month := factor(month.text, month.levs)]
velos.dt[, month.POSIXct := as.POSIXct(
  strptime(paste0(month.str, "-15"), "%Y-%m-%d"))]
counts.per.month <- velos.dt[, list(
  count=sum(count)
), by=.(location, month, month.str, month.POSIXct)][0 < count,]
month.labels <- counts.per.month[, {
  .SD[count==max(count), ]
}, by=location]
city.wide.cyclists <- counts.per.month[0 < count, list(
  count=sum(count)
), by=.(month, month.str, month.POSIXct)]
city.wide.cyclists[, month01.str := paste0(month.str, "-01")]
city.wide.cyclists[, month01.POSIXct := as.POSIXct(strptime(month01.str, "%Y-%m-%d"))]
city.wide.cyclists[, next.POSIXct := month01.POSIXct + one.day * 31]
city.wide.cyclists[, next01.str := paste0(strftime(next.POSIXct, "%Y-%m"), "-01")]
city.wide.cyclists[, next01.POSIXct := as.POSIXct(strptime(next01.str, "%Y-%m-%d"))]

##dput(RColorBrewer::brewer.pal(10, "Reds"))
severity.colors <- 
  c("#FFF5F0",#lite red
    "people slightly injured"="#FEE0D2",
    "#FCBBA1",
    "#FC9272",
    "people severely injured"="#FB6A4A",
    "#EF3B2C", 
    "#CB181D",
    deaths="#A50F15",
    "#67000D")#dark red
ggplot()+
  geom_tallrect(aes(xmin=month01.POSIXct, xmax=next01.POSIXct,
                    clickSelects=month),
                data=months, alpha=1/2)+
  scale_fill_manual(values=severity.colors)+
  geom_bar(aes(month.POSIXct, people, fill=severity),
           stat="identity",
           color="black",
           data=accidents.tall)

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
  guides(color="none", fill="none")+
  theme_bw()+
  theme_animint(width=1000)+
  geom_tallrect(aes(xmin=month01.POSIXct, xmax=next01.POSIXct,
                    clickSelects=month),
                data=months, alpha=1/2)+
  geom_line(aes(month.POSIXct, count, group=location,
                color=location,
                showSelected=location,
                clickSelects=location),
            data=counts.per.month)+
  scale_color_manual(values=location.colors)+
  scale_fill_manual(values=location.colors)+
  xlab("month")+
  ylab("bike counts per month")+
  geom_point(aes(month.POSIXct, count, fill=location,
                 tooltip=paste(
                   count, "bikers counted at",
                   location, "in", month),
                 showSelected=location,
                 clickSelects=location),
             size=5,
             color="black",
             data=counts.per.month)+
  geom_text(aes(month.POSIXct, count+5000, color=location, label=location,
                showSelected=location,
                clickSelects=location),
            data=month.labels)
print(MonthSeries)

MonthFacet <- 
  ggplot()+
  ggtitle("counts per month, select month")+
  guides(color="none")+
  theme_bw()+
  facet_grid(facet ~ ., scales="free")+
  theme(panel.margin=grid::unit(0, "lines"))+
  theme_animint(width=1000)+
  geom_tallrect(aes(xmin=month01.POSIXct, xmax=next01.POSIXct,
                    clickSelects=month),
                data=data.table(city.wide.cyclists,
                                facet="cyclists at counters"),
                alpha=1/2)+
  geom_line(aes(month.POSIXct, count, group=location,
                color=location,
                showSelected=location,
                clickSelects=location),
            data=data.table(counts.per.month, facet="cyclists at counters"))+
  scale_color_manual(values=location.colors)+
  xlab("month")+
  ylab("")+
  geom_point(aes(month.POSIXct, count, color=location,
                 tooltip=paste(
                   count, "cyclists counted at",
                   location, "in", month),
                 showSelected=location,
                 clickSelects=location),
             size=5,
             fill="grey",
             data=data.table(counts.per.month, facet="cyclists at counters"))+
  geom_text(aes(month.POSIXct, count+5000, color=location, label=location,
                showSelected=location,
                clickSelects=location),
            data=data.table(month.labels, facet="cyclists at counters"))+
  scale_fill_manual(values=severity.colors, breaks=rev(severity.vec))+
  geom_bar(aes(month.POSIXct, people, fill=severity),
           stat="identity",
           color=NA,
           data=data.table(accidents.tall, facet="city-wide accidents"))+
  geom_tallrect(aes(xmin=month01.POSIXct, xmax=next01.POSIXct,
                    tooltip=paste(
                      ifelse(deaths==0, "",
                             ifelse(deaths==1,
                                    "1 death, ",
                                    paste(deaths, "deaths, "))),
                      ifelse(people.severely.injured==0, "",
                             ifelse(people.severely.injured==1,
                                    "1 person severely injured, ",
                                    paste(people.severely.injured,
                                          "people severely injured, "))),
                      people.slightly.injured,
                      "people slightly injured in",
                      month),
                    clickSelects=month),
                alpha=0.5,
                data=data.table(accidents.per.month,
                                facet="city-wide accidents"))


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
    ##MonthSeries=MonthSeries,
    MonthFacet=MonthFacet,
    ##summary=LocSummary+guides(color="none"),
    summary=MonthSummary,
    map=mtl.map,
    selector.types=list(location="multiple"),
    ##time=list(variable="month", ms=2000),
    duration=list(month=2000),
    first=list(location=c("Berri", "Rachel")),
    title="Montreal cyclists, 2009-2013")

animint2dir(viz, "figure-timeseries")

##animint2gist(viz)
