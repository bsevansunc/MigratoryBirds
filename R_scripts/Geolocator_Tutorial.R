######################################################################################
######################################################################################
#
#
#          Ecology and Conservation of Migratory Birds
#                Tutorial for Geolocator Analysis 
#
#
######################################################################################
######################################################################################

# Remove everything files from R console 
# remove(list=ls())

# Set working directory / where files will be found and saved 
# Note - This will need to modified for your machine 

setwd("C:/Users/MTHallworth/Google Drive/EcologyConservationMigratoryBirds_Geolocator")

# Install packages that are needed for analysis 
# install.packages(c("GeoLight","raster","RColorBrewer","ks"),dependencies=TRUE)

library(GeoLight)
library(raster)

#######################################################################################
#######################################################################################
#######################################################################################
#
#
#   GeoLight example using files from British Antarctic Survey (BAS) with .lig files 
#
#
#######################################################################################
#######################################################################################
#######################################################################################
# Analysis of geolocator data using GeoLight #
# function written by Simon Wotherspoon accessed from GitHUB
source("read_lig.R")

# Import .lig file using read.lig fucntion to convert dates that GeoLight recognizes
GL_68517<-read.lig("2391_68517_000.lig") 

# View first few rows of data 
head(GL_68517)


#################### Define modified twilightCalc function ###########################
source("twilightCalc2.R")

# Calculate Transitions - the times used to generate location estimates
# Note - make sure you save the twilightCalc function to an object -
#        otherwise data are 'lost' in the console. Also, be sure to 'end'
#        when you get to the last transition - otherwise all your work may
#        be lost. 

# In this example - a threshold of 5 was used - a larger value can be used but 
#                   it will increase the number of transitions that need to be scored.

# LightThreshold - determines light levels over 5 "sun has risen/set" and asks
#                  you to accept/reject them

# Note - determining the transitions in this file took approx. 45mins - 1hr  

GL_68517_transitions_example<-twilightCalc2(datetime = GL_68517[,2],
                               light= GL_68517[,4],
                               LightThreshold=5, 
                               ask=TRUE)

# write.csv(GL_68517_transitions,"GL_68517_transitions.csv")

# Read in file of transitions - previously determined by MTH.
GL_68517_transitions<-read.csv("GL_68517_transitions.csv")

# View first few rows of the data
head(GL_68517_transitions)
    
# Calculate the sun-elevation angle of a known capture location
# Using only transitions when you know the bird was at the capture location 
# - here I chose between deployment date and July 31. 
# The coordinates also need to be entered - (X,Y) in that order. 
# This bird was captured at Hubbard Brook Experimental Forest in NH  
 
getElevation(tFirst= GL_68517_transitions[1:102,2],
             tSecond= GL_68517_transitions[1:102,3],
             type=GL_68517_transitions[1:102,4],
             known.coord=c(-71.45,43.945),
             plot=TRUE)

# -1.725 

######################################################################################
#         Naive location estimates assuming no change in sun elevation angle         #
#                          throughout the year                                       #
######################################################################################

GL_68517Locations<-coord(tFirst= GL_68517_transitions[,2],
                         tSecond= GL_68517_transitions[,3],
                         type=GL_68517_transitions[,4], 
                         degElevation=-1.725)

# View first few rows of the file #
head(GL_68517Locations)

# read in world shapefile to get a feel for geographic locations
world<-shapefile("TM_WORLD_BORDERS/TM_WORLD_BORDERS-0.3.shp")


# Plot country boundries then add a plot of the locations #
plot(GL_68517Locations, pch="*", col="red")
plot(world,add=TRUE)


#######################################################################################
#                                                                                     #
# Determine sun elevation angles for different times of the year                      #
#                                                                                     #
#######################################################################################
# First manually determine breeding - non-breeding periods based on natural history
# Create empty column to later fill with numeric values for period - 
#        0= Migratory Period,, 1=Breeding, 2=Non-breeding 

GL_68517_transitions$period<-rep(NA,672)

head(GL_68517_transitions)

# Define Breeding based on natural history of species  
# Ideally this would be done based on the biology (breeding season) and so on
#            breeding = from capture to July 31
#        non-breeding = 1 Nov - 31 March
# see Hallworth et al. in press for details 

names(GL_68517_transitions)
GL_68517_transitions[1:102,5]<-1
GL_68517_transitions[103:287,5]<-0
GL_68517_transitions[288:534,5]<-2
GL_68517_transitions[535:672,5]<-0

# Generate sun-elevation angles for different periods of the
# year using Ekstrom-Hill calibration   
# Generates a sun-elevation angle for all non-zero periods 
# 

HillEkstromCalib(tFirst= GL_68517_transitions[,2],
                 tSecond= GL_68517_transitions[,3],
                 type=GL_68517_transitions[,4],
                 site=GL_68517_transitions[,5],
                 start.angle=c(0,-3), #angle to start  process c(breeding,nonbreeding)
                 plot=TRUE)

# Define a column using the sun elevation angles generated
# using known location and Ekstrom-Hill calibration for unknown locations

GL_68517_transitions$sun<-rep(NA,672)
names(GL_68517_transitions)
GL_68517_transitions[1:102,6]<-(-1.725)
GL_68517_transitions[103:287,6]<-(0) # Migration values are zero in this example
GL_68517_transitions[288:534,6]<-(-2.1)
GL_68517_transitions[535:672,6]<-(0) # Migration values are zero in this example

# Note - One could treat migration points differently - for example, the midpoint
#        between breeding and non-breeding sun elevations could be used as a
#        transition period (this would need to be justified in your manuscript)
#        or you could use either the breeding or non-breeding sun angle but also
#        would need to be justified. 


# Generate new location data based on newly derived sun-elevation angles 
# Note- I would use the true sun elevation angle for the deployment site
#       and the derived sun elevation angle for the non-breeding site.

GL_68517LocationsEHC<-coord(tFirst= GL_68517_transitions[,2],
                        tSecond= GL_68517_transitions[,3],
                        type=GL_68517_transitions[,4], 
                        degElevation=GL_68517_transitions$sun,
                        sites=GL_68517_transitions[,5])

# Plot the new location data #

plot(GL_68517LocationsEHC, pch="*", col="red")
plot(world,add=TRUE)   

# Note - in this example the difference between breeding and non-breeding was not that
#        large. Therefore, the locations derived using the two methods are very similar
#        this may not always be the case. If you don't know the sun-elevation angle for
#        the different stages of the annual cycle, I would highly recommend using the
#        EkstromHill calibration instead of only using a single sun-elevation angle
#        for the entire year. Many factors can influence sun elevation angle including
#        habitat, topography, weather, behavior, etc. Many of these factors differ btwn
#        different phases of the annual cycle. 

#######################################################################################
#                                                                                     #
#      Define stationary periods using the data themselves                            # 
#                                                                                     #
#######################################################################################

# example - after defining residency periods you would recalculate the sun-elevation
#           angles for each stationary period. This would be helpful for species that
#           move a lot during breeding/winter or have multiple staging areas such as
#           Bobolinks, or Veery for example
# In this example the "periods" are created likely because of shading events in the data
# and probably do not represent transition periods throughout the year.

Residency_68517<-changeLight(tFirst=GL_68517_transitions[,2],
                             tSecond=GL_68517_transitions[,3],
                             type=GL_68517_transitions[,4],
                             quantile=0.95,# High value for less stationary periods
                             days=15)      #Increase/decrease min. duration of period

#######################################################################################
#
#      Create Kernel Density Estimates (KDE) around the stationary portions 
#
#######################################################################################
library(ks)
library(raster)
library(RColorBrewer)

# Breeding locations were determined using both locations from earlier 
breeding_68517<-data.frame(GL_68517Locations[1:102,1],GL_68517Locations[1:102,2])

# Determine bandwidth for the Kernel density estimate #
Bwidth<-Hlscv(breeding_68517)

# Create kernel density estimate #
# this creates a raster of the KDE to plot various colors
Breeding_KDE<-raster(kde(x=breeding_68517,H=Bwidth)) 


# Non-breeding locations were determined locations from earlier.
NB_68517<-data.frame(GL_68517LocationsEHC[288:534,1],GL_68517LocationsEHC[288:534,2])
NBwidth<-Hlscv(NB_68517)
NonBreeding_KDE<-raster(kde(x=NB_68517,H=NBwidth))

#Color the values in the KDE similar for breeding and non-breeding
Breed.breaks<-seq(from=0,to=maxValue(Breeding_KDE),(maxValue(Breeding_KDE)/100))
NB.breaks<-seq(from=0,to=maxValue(NonBreeding_KDE),(maxValue(NonBreeding_KDE)/100))

# Plot the results - not elegant but you can dress it up from here anyway you want
# once you have the location data 

plot(world, ylim=c(-9.22,51.15),xlim=c(-107.7,-63.42))
plot(Breeding_KDE,
     axes=FALSE,
     breaks=Breed.breaks,
     col=colorRampPalette(brewer.pal(9,"Blues"))(100),
     legend=FALSE,add=TRUE)
plot(NonBreeding_KDE,
     axes=FALSE,
     breaks=NB.breaks,
     col=colorRampPalette(brewer.pal(9,"Blues"))(100),
     legend=FALSE,add=TRUE)
plot(world,add=TRUE)


#######################################################################################
#######################################################################################
#
#
#     Reading in geolocator data files from Migrate Tech (.lux) files
#
#
#######################################################################################
#######################################################################################
# Transform .lux files to Geolight files

NameObject<-luxTrans("Path_to.lux_file")

# Procced with analysis detailed above

#######################################################################################
#######################################################################################
#
#
#     Reading in geolocator data files from Lotek LightBug
#
#
#######################################################################################
#######################################################################################
# Transform data to GeoLight files
source("read_Lightbug.R")
WOTH<-read.LightBug("0309_RAW.txt")
head(WOTH)
tail(WOTH)

# Note - the light scale is different on the LightBug - starts at 206 
       - The threshold was set accordingly

WOTH_transitions_example<-twilightCalc2(datetime = WOTH[,4],
                               light= WOTH[,3],
                               LightThreshold=265, 
                               ask=TRUE)

#write.csv(WOTH_transitions_example,"WOTH_transitions.csv")

WOTH_transitions<-read.csv("WOTH_transitions.csv")
head(WOTH_transitions)


#######################################################################################
#######################################################################################
#
#
#     Reading in TRJ files where transitions and locations were determined using
#                 TransEDIT, LocatorAid and Locator 
#
#
#######################################################################################
#######################################################################################
source("read_trj.R")

# read.trj(file=,SunAngle=) 
# file="path to .trj file"
# SunAngle= The sun elevation angle used to determine locations in Locator - this is
#           included to remind the user and to potentially subset data if multiple
#           sun elevation angles are used. Note - one sun elevation angle per object
OVEN_TRJ<-read.trj("2391_68517_both.trj", SunAngle =-3)
head(OVEN_TRJ)
tail(OVEN_TRJ)

######################################################################################
######################################################################################
######################################################################################
#
#  Sample code for most probable migration routes + 95% CI around that route
#
######################################################################################
######################################################################################
######################################################################################

# Note - You may need a version of R that is earlier than R 3.0.0 for this code to work#
#        In order for this to work in version 3.0.0 or higher you need to make sure
#        there are no spaces in either the directory to R or your working directory
 
# before proceeding run the code entitled install_KFTrack and install package gmt #

library(kftrack)
library(date)
library(gmt)

Spring25044<-read.csv("Spring25044.csv", sep=",", header=TRUE)

# Error in X and Y directions were estimated from known capture locations of breeding individuals
# where the points were compared to actual breeding coordinates to determine error

# b.x_mean<-1.682        #mean difference estimates
# b.y_mean<-(-0.681)     #mean difference estimates
# s.x_mean<-2.078        #mean standard deviation estimates
# s.y_mean<-2.914        #mean standard deviation estimates


Spring25044<-Spring25044[c(1:3,5:14),]   # removes a data point that was wildly different from the rest 
Spring25044_Track<-kftrack(data = Spring25044, fix.first=TRUE, fix.last=TRUE, 
  u.active=TRUE,  u.init=0,
  v.active=TRUE,  v.init=0,
  D.active=TRUE, D.init=100,
  bx.active=FALSE, bx.init=0.014,  #
  by.active=FALSE, by.init=-0.229, # These are based on error around breeding location
  sx.active=FALSE, sx.init=1.72,   #
  sy.active=FALSE, sy.init=1.42,   #
  a0.active=TRUE, a0.init=0.001,
  b0.active=TRUE, b0.init=0,
  vscale.active=TRUE, vscale.init=1,   
  var.struct="uniform", dev.pen=0.0)
plot(Spring25044_Track)

