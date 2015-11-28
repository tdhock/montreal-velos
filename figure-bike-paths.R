works_with_R("3.2.2", data.table="1.9.6", ggplot2="1.0")

load("bike.paths.RData")
load("counter.locations.RData")

counter.locations[, lon := coord_X]
counter.locations[, lat := coord_Y]

gg <- ggplot()+
  ggtitle("Montreal bike paths and counters")+
  coord_equal()+
  scale_size_manual(values=c(winter=2, "not winter"=0.5))+
  geom_path(aes(lon, lat, color=TYPE_VOIE,
                size=SAISONS4,
                group=paste(feature.i, path.i)),
            data=bike.paths)+
  geom_point(aes(lon, lat, fill=Type),
             shape=21,
             color="black",
             data=counter.locations)+
  geom_text(aes(lon, lat, label=gsub("[^ a-zA-Z]", "", nom)),
            hjust=1,
            data=counter.locations)

png("figure-bike-paths.png")
print(gg)
dev.off()
