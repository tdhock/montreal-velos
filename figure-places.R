works_with_R("3.2.2",
             "tdhock/animint@4257e8cf76eb5021a98010b6629b779a4f383b24",
             "tdhock/ggplot2@a8b06ddb680acdcdbd927773b1011c562134e4d2",
             data.table="1.9.6")

load("places.RData")

places.dt <- data.table(places)
places.dt[, piste := 
  ifelse(grepl("RACHEL", street), "Rachel", "Maisonneuve") ]

viz.prefix <- list(
  map=ggplot()+
    coord_equal()+
    ggtitle(paste("Bike accidents in Montreal, 2002-2004",
                  "Rachel and Maisonneuve only",
                  sep="\n"))+
    geom_point(aes(lon, lat, color=type,
                   tooltip=query.prefix,
                   shape=piste),
               size=5,
               data=places.dt))
png("figure-places-prefix.png")
print(viz.prefix$map)
dev.off()
animint2dir(viz.prefix, "figure-places-prefix")

table(places$type) ## are approximate places a problem?

places.dt[, dist.from.montreal := sqrt((lon+73.577515)^2+(lat-45.49806)^2)]
places.dt[order(-dist.from.montreal), ]
places.dt[, date :=
  as.POSIXct(strptime(date.str, "%Y-%m-%d"))]
places.dt[, month.str := strftime(date, "%Y-%m-01")]
places.dt[, month := as.POSIXct(strptime(month.str, "%Y-%m-%d"))]
places.dt[, severity := ifelse(
  0 < deaths, "death", ifelse(
    0 < people.severely.injured, "severe injury", "slight injury"))]
                                   
accident.counts <-
  places.dt[, list(accidents=.N), by=.(piste,month,severity)]

people.counts <- places.dt[, data.table(
  people=c(sum(people.slightly.injured),
           sum(people.severely.injured),
           sum(deaths)),
  severity=c("slight injury",
             "severe injury",
             "death")),
  by=.(piste,month)]

people.points <- people.counts[, {
  severity.vec <- rep(severity, people)
  severity.sorted <- sort(severity.vec)
  data.table(
    person=seq_along(severity.sorted),
    severity=severity.sorted)
}, by=.(piste,month)]

points.per.path <- people.points[, list(people=.N), by=piste]

##dput(RColorBrewer::brewer.pal(10, "Blues"))
severity.colors <-
  c("#F7FBFF",#lite blue
    "slight injury"="#DEEBF7",
    "#C6DBEF",
    "#9ECAE1",
    "severe injury"="#6BAED6",
    "#4292C6", 
    "#2171B5",
    "death"="#08519C",
    "#08306B")#dark blue

## categorical:
  c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E")

timeSeries.facets <- ggplot()+
  scale_fill_manual(values=severity.colors)+
  facet_grid(piste ~ .)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))+
  geom_bar(aes(month, accidents, fill=severity),
           stat="identity",
           position="stack",
           data=accident.counts)
png("figure-places-timeSeries-facets.png")
print(timeSeries.facets)
dev.off()

timeSeries.facets.people <- ggplot()+
  scale_color_manual(values=severity.colors)+
  facet_grid(piste ~ .)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))+
  geom_point(aes(month, person, color=severity),
             data=people.points)
png("figure-places-timeSeries-facets-people.png")
print(timeSeries.facets.people)
dev.off()

## TODO: get real bike path data.
bike.paths <- places.dt[!is.na(lat), list(
  lat=range(lat),
  lon=range(lon)),
  by=piste]

seconds.in.a.day <- 60 * 60 * 24
months <- people.points[, list(
  people=.N,
  prev.15=month-15*seconds.in.a.day,
  next.15=month+15*seconds.in.a.day
), by=month]

accident.months <- places.dt[, list(accidents=.N), by=month]

viz <- list(
  title="Montreal bike accidents, 2002-2004",
  map=ggplot()+
    guides(fill="none")+
    coord_equal()+
    theme_bw()+
    ## theme(axis.line=element_blank(), axis.text=element_blank(), 
    ##       axis.ticks=element_blank(), axis.title=element_blank())+
    ggtitle("Map of Montreal")+
    geom_text(aes(-73.58, 45.55, label=sprintf(
      "%d accident%s in %s", accidents,
      ifelse(accidents==1, "", "s"),
      strftime(month, "%b %Y")),
      showSelected=month),
      data=accident.months)+
    scale_fill_manual(values=severity.colors)+
    ylab("lat")+
    xlab("lon")+
    geom_path(aes(lon, lat,
                  group=piste,
                  clickSelects=piste),
              alpha=0.6,
              size=5,
              data=bike.paths)+
    geom_point(aes(lon, lat,
                   showSelected=month,
                   showSelected2=severity,
                   fill=severity),
               color="black",
               size=5,
               data=places.dt),
  timeSeries=ggplot()+
    ggtitle("People injured over time")+
    xlab("month")+
    scale_y_continuous("people injured", breaks=1:10)+
    theme_bw()+
    scale_fill_manual(values=severity.colors)+
    geom_text(aes(strptime("2013-07-01", "%Y-%m-%d"), 10,
                  ## TODO: pluralize?
                  label=sprintf("%d people injured on %s bike path",
                                people, piste),
                  showSelected=piste),
              data=points.per.path)+
    geom_tallrect(aes(xmin=prev.15, xmax=next.15,
                      clickSelects=month),
                  alpha=0.5,
                  data=months)+
    geom_point(aes(month, person,
                   showSelected=piste,
                   fill=severity),
               color="black",
               size=5,
               data=people.points),
  time=list(variable="month", ms=3000),
  selector.types=list(severity="multiple")
  )
animint2dir(viz, "figure-places")
##animint2gist(viz)
