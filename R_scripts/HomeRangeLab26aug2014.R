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
import1 = read.csv("/Users/LLPmac/Documents/AMRE_YEWA/MigratoryBirdsCourse/MigratoryBirds/RawBrazilTelemData26aug2014.csv", 
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

#HERE DISCUSS THEORY AND APPLICATION OF TRIANGULATIONS
 #LOCATE III is a free program for PC to do triangulations, but it is buggy
 #LOAS is a PC-based program to do triangulations - not buggy, but $75
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

-----------------------*
#3D Kernel Data prep----
-----------------------*
#installing required packages
install.packages('ks','MASS', 'KernSmooth', 'CircStats','odesolve','coda','deldir','igraph','RandomFields')

#Loading required packages 
library("ks")
library("MASS")
library("KernSmooth")
library("CircStats")
library("odesolve")  #***not avail for this version of R??! #doesn't seem to matter?
library("coda")
library("deldir")
library("igraph")
library("RandomFields")

#EXCERCISE: remove the column called "rec" from the locs dataset
  #this way the columns will just be X,Y,Z, and bird
##***REMOVE THIS LINE
locs$rec<-NULL

#check the dataset
str(locs)

#EXCERCISE: split the "locs" object into three different object: one per animal
 #hint: you can use subset()

#***REMOVE THE RIGHT SIDE OF THIS & SAVE ELSEWHERE 
a<-subset(locs, bird=="YWFA,Y_G")
b<-subset(locs, bird=="RSFA_BK,O")
c<-subset(locs, bird=="RSMBK,BK_A")

#EXCERCISE: remove the bird column from each of the three new objects, a, b & c
  #the dataset can only have X, Y, and Z
 #***REMOVE THESE LINES BELOW
a$bird<-NULL
b$bird<-NULL
c$bird<-NULL

-----------------------*
  #3D Kernel Data analyses----
-----------------------*

# calls the plug-in bandwidth estimator, there are several types of bandwidth estimators - this is generally accepted as the best
# if your resulting territories are lots of separated pieces you can multiply Hpi(a) by factors larger than 1
Ha <- Hpi(a)
Hb <- Hpi(b)
Hc <- Hpi(c)

#joins 2 datasets and then determines min/mix for each dimension. Important that overlapping territories are evaluated in the same 
#physical space. Adds buffer points so that no part of the territories are cutoff.
all<-rbind(a,b)

minX<-min(all$X)-25
minY<-min(all$Y)-25
minZ<-0

maxX<-max(all$X)+25
maxY<-max(all$Y)+25
maxZ<-max(all$Z)+5

# runs the kernel density analysis, gridsize is the number of voxels (3D version of pixel) that the space is divided up to. 
 #this will take a few seconds to run for each bird
fhata <- kde(x=a, H=Ha, binned=FALSE, xmin=c(minX,minY,minZ), gridsize = 151, xmax=c(maxX,maxY,maxZ)) 
fhatb <- kde(x=b, H=Hb, binned=FALSE, xmin=c(minX,minY,minZ), gridsize = 151, xmax=c(maxX,maxY,maxZ))
fhatc <- kde(x=c, H=Hc, binned=FALSE, xmin=c(minX,minY,minZ), gridsize = 151, xmax=c(maxX,maxY,maxZ))

#calculates isopleth at 95% - cta and ctb just produce a number. I think it is the the cutoff for each isopleth. So all 
# voxels with a density greater than cta would be in that isopleth
cta <- contourLevels(fhata, cont=95, approx=FALSE) 
ctb <- contourLevels(fhatb, cont=95, approx=FALSE)
ctc <- contourLevels(fhatc, cont=95, approx=FALSE)

#calculates the volume of each territory at 95th isopleth
 #if UTMs used, then units should be m^3
Vol95a<-contourSizes(fhata, cont=95)
Vol95b<-contourSizes(fhatb, cont=95)
Vol95c<-contourSizes(fhatc, cont=95)

#plotting the home ranges in 3D
plot(fhata,cont=c(95),colors=("yellow"),drawpoints=TRUE,xlab="", ylab="", zlab="",xlim=c(minX,maxX),ylim=c(minY,maxY),zlim=c(minZ,maxZ),size=2, ptcol="black") 
plot(fhatb,cont=c(95),colors=("red"),add=TRUE,drawpoints=TRUE,xlab="", ylab="", zlab="",size=2,ptcol="red")
plot(fhatc,cont=c(95),colors=("blue"),add=TRUE,drawpoints=TRUE,xlab="", ylab="", zlab="",size=2,ptcol="blue")

#======================================================================*
# ---- Excercises ----
#======================================================================*
#1) Determine the amount of overlap (in 2D) between animal 1 and animal 2.

#2) For the 2D home ranges, change the colors of the animal home ranges to blue, black, and orange

#3) Does anyone know trigonometry? 
    #If so, write a function to calculate the intersection of two animal signals
      #hint, solve the problem first, then write the function
