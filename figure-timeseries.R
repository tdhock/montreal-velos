works_with_R("3.1.1",
             dplyr="0.2",
             "tdhock/animint@0cdf7f2b875c93efb1138e6986979493fda612c6",
             "tdhock/ggplot2@98cefe4d653ce8f214177b66dc030c2f3c725ffb")

load("velos.RData")

one.day <- 60 * 60 * 24

velos <- velos %.%
  filter(!is.na(count))

ggplot()+
  geom_line(aes(date, count),
            data=velos)+
  geom_point(aes(date, count),
             data=velos, pch=1, alpha=1/2)+
  facet_wrap("location")+
  theme(panel.margin=grid::unit(0, "cm"))

dates <- velos %.%
  group_by(date) %.%
  summarise(locations=sum(!is.na(count))) %.%
  filter(locations > 0)

location.labels <- velos %.%
  group_by(location) %.%
  filter(seq_along(count)==which.max(count))

location.ranges <- velos %.%
  group_by(location) %.%
  summarise(min=min(date),
            max=max(date))

LocSummary <- ggplot()+
  xlab("date")+
  geom_tallrect(aes(xmin=date-one.day/2, xmax=date+one.day/2,
                    clickSelects=date),
                data=dates, alpha=1/2)+
  geom_segment(aes(min, location,
                   xend=max, yend=location,
                   color=location,
                   clickSelects=location),
               data=location.ranges, alpha=3/4, size=10)
print(LocSummary)

data.frame(dates)

TimeSeries <- ggplot()+
  theme_animint(width=1000)+
  geom_tallrect(aes(xmin=date-one.day/2, xmax=date+one.day/2,
                    clickSelects=date),
                data=dates, alpha=1/2)+
  geom_line(aes(date, count, group=location,
                showSelected=location,
                clickSelects=location),
            data=velos)+
  geom_point(aes(date, count, color=location,
                 showSelected=location,
                 clickSelects=location),
             data=velos)+
  geom_text(aes(date, count+200, color=location, label=location,
                showSelected=location,
                clickSelects=location),
            data=location.labels)
print(TimeSeries)

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
  list(bars=bars+guides(fill="none"),
       summary=LocSummary+guides(color="none"),
       TimeSeries=TimeSeries+guides(color="none"),

       time=list(variable="date", ms=3000),
       selector.types=list(location="multiple"),
       duration=list(date=1000),
       title="Montreal cyclists, 2009-2013")

animint2dir(viz, "timeseries")

##animint2gist(viz)
