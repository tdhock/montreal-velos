works_with_R("3.2.2", data.table="1.9.6", ggmap="2.5.2")

load("accidents.RData")

some.accidents <- data.table(accidents)[grepl("MAISONNEUV|RACHEL", street),]
some.accidents[, street := sub("FACE A$", "", street)]
some.accidents[, cross.street := sub("^LAURE$", "ANDRE LAURENDEAU",
                                cross.street)]
some.accidents[, 
  query.prefix := ifelse(!is.na(street.number), paste(street.number, street),
    ifelse(grepl(" ET ", street), sub(" ET ", " and ", street),
           paste(street, "and", cross.street))
                          ) ]
some.accidents[, geocode.query := paste0(query.prefix, ", Montreal, Canada")]

if(file.exists("places.RData")){
  load("places.RData")
}else{
  geocode.list <- list()
}

is.missing <- sapply(geocode.list, function(df)is.na(df$lon[1]))
geocode.list <- geocode.list[!is.missing]

new.query.vec <-
  unique(some.accidents[! geocode.query %in% names(geocode.list), geocode.query])
for(new.i in seq_along(new.query.vec)){
  query <- new.query.vec[[new.i]]
  cat(sprintf("%4d / %4d geocoding '%s'\n",
              new.i, length(new.query.vec), query))
  geocode.result <- geocode(query, source="google", output="more")
  geocode.list[[query]] <- geocode.result
  Sys.sleep(1)
}

setkey(some.accidents, geocode.query)
places.list <- list()
for(query in unique(some.accidents$geocode.query)){
  geocode.result <- geocode.list[[query]]
  geocode.some <- geocode.result[, c("lon", "lat", "type", "loctype")]
  places.list[[query]] <-
    data.frame(query,
               some.accidents[query],
               geocode.some)
}
places <- do.call(rbind, places.list)
rownames(places) <- NULL

save(places, geocode.list, file="places.RData")
