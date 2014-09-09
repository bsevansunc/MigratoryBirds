read.LightBug<-function(file,skip=1) {
## Read csv file and add column names
d <- read.table(file,header=F,skip=skip,sep="\t",
col.names=c("time","date","light"),
colClasses=c("character","character","integer"))
## Parse date
d$datetime <- as.POSIXct(strptime(paste(d$date,d$time,sep=" "),"%d/%m/%y %H:%M"),tz="GMT")
d
}