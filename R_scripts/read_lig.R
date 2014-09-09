read.lig <- function(file,skip=1) {
## Read csv file and add column names
d <- read.csv(file,header=F,skip=skip,
col.names=c("Valid","Date","Julian","Light"),
colClasses=c("character","character","numeric","integer"))
## Parse date
d$Date <- as.POSIXct(strptime(d$Date,"%d/%m/%y %H:%M:%S",tz="GMT"))
d
}