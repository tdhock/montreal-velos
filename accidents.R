works_with_R("3.2.2", data.table="1.9.6")

column.code <- c(
  date.str="DT_ACCDN",
  time.str="HR_ACCDN",
  deaths="NB_MORT_C",
  people.severely.injured="NB_BLESE_GRAVE_C",
  people.slightly.injured="NB_BLESE_LEGER_C",
  street.number="NO_CIVIQ_ACCDN",
  ##street.number.suffix="SFX_NO_CIVIQ_ACCDN",
  street="RUE_ACCDN",
  cross.street="ACCDN_PRES_DE",
  location.int="CD_LOCLN_ACCDN",
  position.int="CD_POSI_ACCDN")

## These meaning of these integer codes is described in
## PAC13_p02_Documentation_Accidents_Cyclistes_2012_2014.pdf
code.list <-
  list(position=c(
         "1"="Voie réservée en service",
         "2"="Voie lente / voie de dépassement",
         "3"="Perte / gain de voie",
         "4"="Voie de virage à gauche dans les 2 sens",
         "5"="Voie cyclable / chaussée désignée",
         "6"="Voie de circulation",
         "7"="Accotement (ou bord de la chaussée",
         "8"="Terre-plein central ou îlot",
         "9"="Trottoir",
         "10"="Autre"),
       location=c(
         "31"="Carrefour giratoire / rond-point",
         "32"="En intersection (moins de 5 mètres)",
         "33"="Près d'une intersection/carrefour giratoire",
         "34"="Entre intersections (100 mètres et +)",
         "35"="Passage à niveau",
         "36"="Pont (au-dessus d'un cours d'eau)",
         "37"="Autre pont (viaduc)",
         "38"="Tunnel",
         "39"="Sous un pont ou un viaduc",
         "40"="Centre commercial",
         "99"="Autre")
       )
  
accidents.by.year <- list()
for(year in 2012:2014){
  csv.file <- sprintf("PAC13_p02_Accidents_Cyclistes_%d.csv", year)
  orig.cols <- fread(csv.file, na.strings="ND")
  not.present <- ! column.code %in% names(orig.cols)
  if(any(not.present)){
    print(column.code[not.present])
    stop("some columns not present in data file")
  }
  renamed <- orig.cols[, column.code, with=FALSE]
  setnames(renamed, names(column.code))
  for(new.name in names(code.list)){
    int.name <- paste0(new.name, ".int")
    if(int.name %in% names(renamed)){
      int.vec <- renamed[[int.name]]
      code.vec <- code.list[[new.name]]
      renamed[[new.name]] <- code.vec[paste(int.vec)]
    }
  }
  accidents.by.year[[paste(year)]] <- renamed
}
accidents <- data.frame(do.call(rbind, accidents.by.year))

save(accidents, file="accidents.RData")
