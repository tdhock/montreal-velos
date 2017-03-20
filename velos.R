works_with_R(
  "3.3.3",
  "Rdatatable/data.table@ce00cb14dc844cac1286681a1da670a2d2405b85")

system("head -3 [0-9]*.csv")

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
months <-
  data.frame(locale=strftime(first.days, "%b"),
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
loc.replace <-
  c("Berri1"="Berri",
    "Berri 1"="Berri",
    "Rachel1"="Rachel",
    Mais1="Maisonneuve 1",
    Mais2="Maisonneuve 2",
    CSC="Côte-Sainte-Catherine",
    Parc="du Parc",
    PierDup="Pierre-Dupuy")

velos.list <- list()
"defivelomtl/Comptages cyclistes automatiques par boucles de détection/comptagesvelo2015.csv" #more data.
files <- paste0(2009:2013, ".csv")
for(f in files){
  ## First determine the encoding -- for example
  ## read.csv("2010.csv",encoding="utf8") gives error "invalid
  ## multibyte string 1".
  enc <- tryCatch({
    read.csv(f, encoding="utf8", sep=";", nrow=1)
    "utf8"
  }, error=function(e){
    read.csv(f, encoding="latin1", sep=";", nrow=1)
    "latin1"
  })

  ## Then determine the separator -- if we use the correct separator,
  ## we should get more than 1 column.
  first <- read.csv(f, encoding=enc, sep=";", nrow=1)
  sep <- ifelse(ncol(first)==1, ",", ";")
  df <- read.csv(f, encoding=enc, sep=sep, check.names=FALSE)

  ## Do the same with data.table: no need to specify encoding.
  first <- fread(f, sep=";", nrow=2, header=TRUE)
  sep <- ifelse(ncol(first)==1, ",", ";")
  dt <- fread(f, sep=sep)

  ## Translate dates from long (samedi 01 janv 2011) to numeric
  ## (01/01/2011).
  date.str <- as.character(df$Date)
  if(grepl(" ", date.str[1])){
    date.mat <- do.call(rbind, strsplit(date.str, split=" "))
    day <- date.mat[,2]
    month.french <- date.mat[,3]
    month <- month2int[month.french]
    year <- date.mat[,4]
    date.str <- sprintf("%s/%02d/%s", day, month, year)
  }

  ## Translate date.str to POSIXlt.
  date <- strptime(date.str, "%d/%m/%Y")
  if(any(is.na(date))){
    print(data.frame(date.str, date))
    stop("some NA dates")
  }

  ## The first column is the date, and the others are locations.
  locations <- names(df)[-1]
  for(location in locations){
    count.vec <- df[[location]]
    if(is.null(count.vec))count.vec <- NA

    ## TODO: For 2010.csv, there are spaces for counts larger than
    ## 999, so we need to delete these.
    if(!is.numeric(count.vec)){
      count.vec <- gsub(" ", "", count.vec)
    }

    count <- as.integer(count.vec)
    if(all(is.na(count))){
      cat("ignoring ", location, " in ", f, "\n")
    }else{
      if(location %in% names(loc.replace)){
        location <- loc.replace[[location]]
      }
      velos.list[[paste(f, location)]] <- data.frame(
        location, date, count)
    }
  }
}
velos <- do.call(rbind, velos.list)

table(velos$location)
non.disponible <- subset(velos, grepl("non", location))
stopifnot(nrow(non.disponible) == 0)

save(velos, file="velos.RData")
