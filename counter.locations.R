works_with_R("3.2.2", data.table="1.9.6")

counter.locations <- fread("defivelomtl/Comptages cyclistes automatiques par boucles de détection/localisationcompteursvelo2015.csv")

save(counter.locations, file="counter.locations.RData")
