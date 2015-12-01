works_with_R("3.2.2", data.table="1.9.6", RJSONIO="1.3.0")

reseau <- fromJSON("defivelomtl/Réseau Cyclable/reseaucyclable201511.json")

conversion.list <- list(
  TYPE_VOIE=c(
    "1"="Chaussée désignée",
    "2"="Accotement asphalté",
    "3"="Bande cyclable",
    "4"="Piste cyclable sur rue",
    "5"="Piste cyclable en site propre",
    "6"="Piste cyclable au niveau du trottoir",
    "7"="Sentier polyvalent"),
  SAISONS4=c(
    NON="not winter",
    OUI="winter")) # les autos ne stationnent pas dedans.

bike.paths.list <- list()
for(feature.i in seq_along(reseau$features)){
  cat(sprintf("%4d / %4d features\n", feature.i, length(reseau$features)))
  feature <- reseau$features[[feature.i]]
  property.list <- list()
  for(property in names(conversion.list)){
    code.vec <- conversion.list[[property]]
    orig.property <- feature$properties[[property]]
    property.list[[property]] <- code.vec[[paste(orig.property)]]
  }
  properties <- as.data.table(property.list)
  path.list <- if(feature$geometry$type=="MultiLineString"){
    feature$geometry$coordinates
  }else{
    list(feature$geometry$coordinates)
  }
  for(path.i in seq_along(path.list)){
    coord.mat <- matrix(unlist(path.list[[path.i]]), ncol=3, byrow=TRUE)
    coord.dt <- data.table(lon=coord.mat[,1], lat=coord.mat[,2])
    bike.paths.list[[paste(feature.i, path.i)]] <-
      data.table(properties, feature.i, path.i, coord.dt)
  }
}
bike.paths <- do.call(rbind, bike.paths.list)

save(bike.paths, file="bike.paths.RData")
