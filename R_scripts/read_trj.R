read.trj<-function(file,SunAngle) {
## Read csv file and add column names
d <- read.csv(file,header=F,skip=1,sep=",",
col.names=c("Noon_Midnight","Date","NoonMidnight_Time","SecondsSinceJan_1_1990","Sunrise","Sunset",
 	"Stationary_Latitude","Compensated_Latitude","Longitude","Distance","Heading","Velocity",	"Confidence")
	#SunElev	Negative_Long
,
colClasses=c("character","character","character","numeric","character","character","numeric","numeric","numeric","numeric","numeric","numeric"))

# add negative longitude & sun elevation angle to file
d$Longitude<-d[,9]*(-1)
d$SunAngle<-rep(NA,length(d[,1]))
d$SunAngle[1:length(d[,1])]<-SunAngle

require(raster)
require(sp)

c<-data.frame(cbind(d[,9],d[,7]))
dSPDF<-SpatialPointsDataFrame(c,d)
dSPDF
}
