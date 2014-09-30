works_with_R("3.1.1", ggplot2="1.0")

load("velos.RData")

ggplot()+
  geom_line(aes(date, count, color=location),
            data=velos)+
  geom_point(aes(date, count, color=location),
             data=velos)  

ggplot()+
  geom_line(aes(date, count),
            data=velos)+
  geom_point(aes(date, count),
             data=velos, pch=1)+
  facet_wrap("location")+
  theme(panel.margin=grid::unit(0, "cm"))

