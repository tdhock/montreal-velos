works_with_R("3.2.2",
             "tdhock/animint@4257e8cf76eb5021a98010b6629b779a4f383b24",
             "tdhock/ggplot2@a8b06ddb680acdcdbd927773b1011c562134e4d2",
             data.table="1.9.6")

load("places.RData")

table(places$type) ## are approximate places a problem?

places.dt <- data.table(places)
places.dt[, dist.from.montreal := sqrt((lon+73.577515)^2+(lat-45.49806)^2)]
places.dt[order(-dist.from.montreal), ]

places.df <- data.frame(places)
places.df$piste <-
  ifelse(grepl("RACHEL", places.df$street), "Rachel", "Maisonneuve")
places.df$date <-
  strptime(places.df$date.str, "%Y-%m-%d")

gg <- ggplot()+
  coord_equal()+
  ggtitle(paste("Bike accidents in Montreal, 2002-2004",
                "Rachel and Maisonneuve only",
                sep="\n"))+
  geom_point(aes(lon, lat, color=type, shape=piste),
             data=places.df)

png("figure-places.png")
print(gg)
dev.off()

