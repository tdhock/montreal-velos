works_with_R("3.1.1",
             dplyr="0.2",
             "tdhock/animint@486830a785840339a3c923dec920358b1aeb0110",
             "tdhock/ggplot2@98cefe4d653ce8f214177b66dc030c2f3c725ffb")

load("velos.RData")

one.day <- 60 * 60 * 24

velos <- velos %.%
  filter(!is.na(count))

ggplot()+
  geom_line(aes(date, count),
            data=velos)+
  geom_point(aes(date, count),
             data=velos, pch=1)+
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
             data=location.ranges, alpha=3/4, size=4)

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
  coord_flip()
print(bars)

viz <-
  list(time=list(variable="date", ms=3000),
       bars=bars+guides(fill="none"),
       summary=LocSummary+guides(color="none"),
       TimeSeries=TimeSeries+guides(color="none"),
       selector.types=list(location="multiple"),
       duration=list(date=1000))

animint2dir(viz, "timeseries")

animint2gist(viz)
