works_with_R(
  "3.3.3",
  "Rdatatable/data.table@ce00cb14dc844cac1286681a1da670a2d2405b85")

## Some code that shows why we can't use strptime to read all the
## dates. For example, in the fr_CA.utf8 locale, the month févr is
## undefined, and ends up as NA in R.
date.format <- "%A %d %b %Y"
strftime(Sys.time(), date.format)
system("locale -a")
Sys.setlocale("LC_TIME", "fr_CA")
strftime(Sys.time(), date.format)
first.days.str <- sprintf("%02d/%02d/%4d", 1, 1:12, 2014)
first.days <- strptime(first.days.str, "%d/%m/%Y")
months <- data.frame(
  locale=strftime(first.days, "%b"),
  data=c("janv", "févr", "mars",
         "avr", "mai", "juin",
         "juil", "août", "sept",
         "oct", "nov", "déc"))
months$data.first <- paste("1", months$data, "2014")
months$strptime <- strptime(months$data.first, "%d %b %Y")
print(months)

## So instead, let's just use a named character vector.
month2int <- 1:12
names(month2int) <- months$data
print(month2int)

## TODO: Some locations seem to be the same, but with different names.
loc.replace <- c(
  "Berri1"="Berri",
  "Berri 1"="Berri",
  "Rachel1"="Rachel",
  Mais1="Maisonneuve 1",
  Mais2="Maisonneuve 2",
  CSC="Côte-Sainte-Catherine",
  Parc="du Parc",
  PierDup="Pierre-Dupuy")

files.dt <- data.table(csv=paste0(2009:2013, ".csv"))
velos.dt <- files.dt[, {
  ## First determine the encoding -- for example
  ## read.csv("2010.csv",encoding="utf8") gives error "invalid
  ## multibyte string 1".
  enc <- tryCatch({
    read.csv(csv, encoding="utf8", sep=";", nrow=1)
    "UTF-8"
  }, error=function(e){
    read.csv(csv, encoding="latin1", sep=";", nrow=1)
    "Latin-1"
  })
  two <- fread(csv, sep="\n", nrow=2, header=FALSE)
  sep <- ifelse(grepl(";", two[2]), ";", ",")
  wide <- fread(csv, sep=sep, encoding=enc)
  ## Translate dates from long (samedi 01 janv 2011) to numeric
  ## (01/01/2011).
  date.str <- as.character(wide$Date)
  if(grepl(" ", date.str[1])){
    date.mat <- do.call(rbind, strsplit(date.str, split=" "))
    day <- date.mat[,2]
    month.french <- date.mat[,3]
    month <- month2int[month.french]
    year <- date.mat[,4]
    date.str <- sprintf("%s/%02d/%s", day, month, year)
  }
  ## Translate date.str to POSIXlt.
  wide[, date := suppressWarnings(strptime(date.str, "%d/%m/%Y"))]
  date.na <- wide[is.na(date),]
  if(nrow(date.na)){
    print(date.na)
    browser()
    stop("some NA dates")
  }
  tall <- suppressWarnings(melt(wide, id.vars=c("Date", "date")))
  tall[, no.space := gsub(" ", "", value)]
  tall[, count := suppressWarnings(as.integer(no.space))]
  na.props <- tall[, list(prop.na=mean(is.na(count))), by=variable]
  all.na <- na.props[prop.na==1,]
  if(nrow(all.na)){
    warning(
      "ignoring non-count column(s) ",
      paste(all.na$variable, collapse=", "))
  }
  tall[, location := ifelse(
    variable %in% names(loc.replace),
    loc.replace[paste(variable)],
    paste(variable))]
  tall[!is.na(count), list(location, date, count)]
}, by=csv]

save(velos.dt, file="velos.dt.RData")
