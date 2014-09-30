date.format <- "%A %d %b %Y"
strftime(Sys.time(), date.format)
system("locale -a")
Sys.setlocale("LC_TIME", "fr_CA.utf8")
strftime(Sys.time(), date.format)
first.days.str <- sprintf("%02d/%02d/%4d", 1, 1:12, 2014)
first.days <- strptime(first.days.str, "%d/%m/%Y")
months <-
  data.frame(locale=strftime(first.days, "%b"),
             data=c("janv", "févr", "mars",
               "avr", "mai", "juin",
               "juil", "août", "sept",
               "oct", "nov", "déc"))
month2int <- 1:12
names(month2int) <- months$data

velos <- NULL
files <- Sys.glob("*.csv")
for(f in files){
  enc <- tryCatch({
    read.csv(f, encoding="utf8", sep=";", nrow=1)
    "utf8"
  }, error=function(e){
    read.csv(f, encoding="latin1", sep=";", nrow=1)
    "latin1"
  })
  first <- read.csv(f, encoding=enc, sep=";", nrow=1)
  df <- if(ncol(first)==1){
    read.csv(f, encoding=enc, sep=",", check.names=FALSE)
  }else{
    read.csv(f, encoding=enc, sep=";", check.names=FALSE)
  }

  date.str <- as.character(df$Date)
  if(grepl(" ", date.str[1])){
    date.mat <- do.call(rbind, strsplit(date.str, split=" "))
    day <- date.mat[,2]
    month.french <- date.mat[,3]
    month <- month2int[month.french]
    year <- date.mat[,4]
    date.str <- sprintf("%s/%02d/%s", day, month, year)
  }
  date <- strptime(date.str, "%d/%m/%Y")
  if(any(is.na(date))){
    print(data.frame(date.str, date))
    stop("some NA dates")
  }

  locations <- names(df)[-1]
  for(location in locations){
    count.vec <- df[[location]]
    if(is.null(count.vec))count.vec <- NA
    count <- as.integer(count.vec)
    if(all(is.na(count))){
      cat("ignoring ", location, " in ", f, "\n")
    }else{
      velos <- rbind(velos, data.frame(location, date, count))
    }
  }
}

table(velos$location)
non.disponible <- subset(velos, grepl("non", location))
stopifnot(nrow(non.disponible) == 0)

save(velos, file="velos.RData")
