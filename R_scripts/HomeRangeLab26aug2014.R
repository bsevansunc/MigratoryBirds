#######################################################################*
# ---- MIGRATORY BIRD COURSE: HOME RANGE LAB ---- 
#######################################################################*
# Title: Home Range Lab
# Author: Luke L. Powell
# Date created: 25 Aug 2014
# Overview: In this lab you will learn to calculate animal home ranges
#  (both minimum convex polygon and kernel density estimator) using the 
#  adihabitatHR package in R. 
#  You will also learn to calculate home ranges in 3D

#======================================================================*
# ---- Set-up ----
#======================================================================*

# Install packages and read libraries:

  install.packages('adehabitatHR','maptools')

  library('adehabitatHR')
  library('maptools')

#set the working directory; 
setwd("/Users/LLPmac/Documents/AMRE_YEWA/MigratoryBirdsCourse/HRlab/")

# import the bird location data
  #data are in UTM format; units are meters
  #the "na.strings" part replaces missing values with "NA"

import = read.csv("/Users/LLPmac/Documents/AMRE_YEWA/MigratoryBirdsCourse/MigratoryBirds/data/RSHomeRangeData.csv", 
                  header = T, na.strings=c("NA", "NULL", "", "."))
 locs<-import

#taking a quick look at the data for each bird
  table(locs$bird)


#======================================================================*
# ---- Prepare for triangulations ----
#======================================================================*
import1 = read.csv("/Users/LLPmac/Documents/AMRE_YEWA/MigratoryBirdsCourse/HRlab/RawBrazilTelemData26aug2014.csv", 
  header = T, na.strings=c("NA", "NULL", "", "."))

telem<-import1

#raw vs. adjusted azmuth (compass bearing)
 #can calculate declanation and adjust declination here: http://www.ngdc.noaa.gov/geomag-web/#declination
 #either use an uncorrected compass and make corrections later
  #or use one of those compasses with which you can adjust declination on the compass itself

#conditions that affect radioteletry:
  #topography
  #wet vegetation
  #distance to animal
  #size of transmitter
  #animal behavior
  #
#======================================================================*
# ---- Prepare the data for adehabitatHR ----
#======================================================================*

#Pulling off just the X and Y data
  xyt<-subset(locs, select = c(X,Y))

#Pulling off just the bird IDs
  id<-subset(locs, select = bird)

#creating a SpatialPointsDataFrame for adihabitatHR
  locs1<-id
  coordinates(locs1)<-xyt
  class(locs1)

#plotting the data
  plot(locs1, col=as.data.frame(locs1)[,1])

#======================================================================*
# ---- Minimum Convex Polygons ----
#======================================================================*

#Running the 95% miminum convex polygon analysis
  cp<-mcp(locs1[,1], percent=95)

#graphing the polygons and the points
  plot(cp)
  plot(locs1, col=as.data.frame(locs1)[,1], add=T)

#area - listed in hectares (ha)
  cp

#write the polygons to a shapefile in the source directory
  writePolyShape(cp,"mcp")

#======================================================================*
# ---- Kernel Density Estimation in 2D----
#======================================================================*

#This section teaches you how to calculate home range size using the kernel density estimator
 #It uses a utilization distribution (ud) to calculate home range size based on the distribution of the points
 #can calculate home range size and 

#running the kernel density estimation (kde) using LSCV (least square cross validataion)
  #h is the smoothing parameter
  #this produces a raster
  kud<-kernelUD(locs1[,1], h="LSCV")

#converting the KDE as vectors
  homerange <- getverticeshr(kud)
  class(homerange)

#plotting the vector
  plot(homerange,col=1:3) 

#Calculating area for different isopleths (utilization distributions[UDs])
  #areas are in hectares
  #95 is the standard for "home range size"
  #50 is often used for the "core area"
  kde.areas<- kernel.area(kud, percent=c(50,95))
  kde.areas

#plotting different isopleths
  #second animal only
  vud<-getvolumeUD(kud)
  image(vud[[2]])
  xyzv <- as.image.SpatialGridDataFrame(vud[[2]])
  contour(xyzv, add=TRUE)

#home range using href instead of LSCV
  kud1 <- kernelUD(locs1[,1], h="href")

#converting the 95 KDE as vectors
  homerange1 <- getverticeshr(kud1, percent = 95)

#plotting the vector and the points
  plot(homerange1,border=1:3, lwd=6) 
  plot(locs1, col=as.data.frame(locs1)[,1], add=T)

#export the shapefiles for the href version of the home ranges
  writePolyShape(homerange1,"95kde")

#converting the 50 KDE as vectors, the "core area
  core <- getverticeshr(kud1, percent = 50)

#plotting the vector and the points
  plot(core,border=1:3, lwd=4, lty = "dashed", add = T) 

#export the shapefiles for the href version of the home ranges
  writePolyShape(core,"50kde")

#======================================================================*
# ---- 3D Kernel Density Estimation ----
#======================================================================*
#this code originally written by Nathan Cooper (Smithsonian Migratory Bird Center)
  #subsequently modified for this course 



#======================================================================*
# ---- Excercises ----
#======================================================================*
#1) Determine the amount of overlap (in 2D) between animal 1 and animal 2.

#2) For the 2D home ranges, change the colors of the animal home ranges to blue, black, and orange

#3)
