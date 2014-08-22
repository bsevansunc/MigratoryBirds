#######################################################################*
# ---- MIGRATORY BIRD COURSE: PHENOLOGY LAB ---- 
#######################################################################*
# Title: Phenology of the Indigo bunting, 1990-2009
# Author: Brian Evans, modified from Hurlbert and Liang 2012
# Date created: 20 Aug 2014
# Overview: This script takes students through the steps associated with
# a phenological analysis of Indigo bunting, a neotropical migrant that 
# can be found in fields and forest edges throughout Eastern North 
# America. Students will:
#   1) Visually explore observations by spatial location and date
#   2) Model the "arrival date" for a given location and year as the 
#      inflection point of a logistic curve between observation and date.
#   3) Explore how the inflection point varies annually.

#======================================================================*
# ---- Set-up ----
#======================================================================*

# Install packages and read libraries:

install.packages('raster', 'maps', 'inflection')

library(raster)
library(maps)
library(inflection)

# Load source functions:

source('/Users/bsevans/Desktop/Migratory_Bird_Course/Phenology_lecture/MigrationSourceFunctions.R')

# Set working directory:

setwd("/Users/bsevans/Desktop/hurlbert_liang/data")

# Gather observations and sampling effort data:

  obs = read.csv('observations.csv')
  samp = read.csv('sampling.csv')

# We are only interested in a few of the columns, so let's get rid of some data:

  names(obs)

  obs = obs[,c(2,4,7:9,12)]

  head(obs)

# Let's change the name of the sample event identifier column to simply sampleID

  names(obs)[1] = 'SampleID'

  test = unique(obs$Scientific.Name)

  list.files()

# Extract observations of Indigo bunting:

  inbu = obs[obs$Scientific.Name == 'Passerina cyanea',]

# Explore the Indigo bunting data:

summary(inbu)

hist(inbu$JulianDay, col = 'gray',
     xlab = 'Julian day', ylab = 'Number of observations',
     main = 'Histogram of observations by Julian day')

# We will now subset this to unique spatial locations by removing the sampleID
# column and subsetting the data to unique records. We can double-check that the
# data were subset using the "dim" function and looking at the row count:

  dim(inbu)

  inbu = inbu[,-1]

  inbu = unique(inbu)

  dim(inbu)

# The inbu dataframe is now the number of spatially-distinct eBird checklists
# that included inbu observations for a given date.

# Let's now determine the number of spatially-distinct eBird checklists for 
# a given date. We will do this similarly to above, are now only interested
# in the lat, lon, year, and Julian date columns.

  names(samp)
  lists = samp[,c(4:6,9)]
  lists = unique(lists)

  dim(samp)
  dim(lists)

#======================================================================*
# ---- Proportion of inbu checklists ----
#======================================================================*
# Goal: Calculate the proportion of eBird checklists that contain inbu
# observations within a 2-degree resolution raster grid cell for an
# example date (we'll use the median Julian day).

#----------------------------------------------------------------------*
# Count the number of inbu observations per grid cell
#----------------------------------------------------------------------*

# Subset the data frame:

  inbu05 = inbu[inbu$Year == 2005 & inbu$JulianDay == 150,]

# Create a data frame of points and observations:

  pts.df = data.frame(inbu05$Longitude, inbu05$Latitude)

# Convert points to spatial:

  pts.sp =  SpatialPoints(pts.df, proj4string = CRS('+proj=longlat +datum=WGS84'))

# Make an empty raster from an extent object:
  
  e = extent(min(lon), max(lon), min(lat), max(lat))
  
  r = raster(e, resolution = 2)

# Define the raster projection:

  projection(r) =  '+proj=longlat +datum=WGS84'

# Count the number of observations within a grid cell:

  observations.ras = rasterize(pts.sp,r, fun = 'count', background = 0)

# Set the raster color scale:

  col.scale = rev(terrain.colors(4))
             
# Take a look:

  plot(observations.ras, col = col.scale, zlim = c(0,20))

# Add states for geographic reference:

  map('state', add = T)

# You can use the "click" function to interactively determine
# the value of a raster cell:

  #click(observations.ras)

#----------------------------------------------------------------------*
# ---- Count the number of eBird lists per grid cell ----
#----------------------------------------------------------------------*

# Now you! Follow the steps above to calculate the number of spatially 
# unique eBird checklists per grid cell on Julian day 150 of 2005 and 
# plot the map.

#----------------------------------------------------------------------*
# ---- Calculate the proportion of inbu lists ----
#----------------------------------------------------------------------*

# Create new raster map of proportional counts:

  inbu.prop = observations.ras/lists.ras

# Reset the color scale and plot

  col.scale = rev(terrain.colors(99))

  plot(inbu.prop, col = col.scale, zlim = c(0,1))

  map('state', add = T)

#======================================================================*
# ---- Observe counts across Julian days for 2005 ----
#======================================================================*

# The steps above are condensed into a single function called 
# "prop.by.DayYear" located on the source functions file.

  prop.by.DayYear

# Let's use the new function to plot a raster map of the number of 
# observations on day 150:

  plot(prop.by.DayYear(150, 2005),
       col = col.scale, zlim = c(0,1))
  map('state', add = T)

# We can now cycle through all of the Julian days in 2005 and watch
# the migration happen!

  jdays = seq(min(obs$JulianDay),max(obs$JulianDay))

  for (i in 1:length(jdays)){
    plot(prop.by.DayYear(jdays[i], 2005), 
         col = col.scale, zlim = c(0,1),
         main = paste('Julian day = ', jdays[i]))
    map('state', add = T)
  }


#======================================================================*
# ---- Proportion of observations across years and pixels ----
#======================================================================*

# We'll now run the for loop again, but this time to extract a data frame
# from each raster.

# Create an empty list:

  l1 = list()

# Run a for loop to extract the proportional observation for each site and 
# day:

  for (i in 1:length(jdays)){
    # Extract values as vectors in the list:
    l1[[i]] = as.data.frame(prop.by.DayYear(jdays[i], 2005))[,1]
  }

# We now have a list where each list item is a vector of proportional
# observations for a given map pixel. Our goal, however, is to write a 
# code that determines how proportional observations change over time
# at a given site.
# 

# Let's use the map pixel that includes Front Royal, Virginia.

# First, we have to find the cell assignment. To do so, we will plot
# the raster and then use the click function to select the appropriate
# cell with our mouse.

# Add the map: 

  r = inbu.prop
  plot(r)
  map('state', add = T)

# Determine the cell reference:

  #click(r, cell = T)

# Calculate the proportion of lists that contain Indigo buntings for 
# Front Royal VA:
  
  prop.of.lists = numeric()
  for (i in 1:length(l1)){
    prop.of.lists[i] = l1[[i]][105]
  }

# Add a column of date values:

  prop.of.lists.df = data.frame(jdays,prop.of.lists)

# Remove the NA's:

  prop.of.lists.df = prop.of.lists.df[!is.na(prop.of.lists.df[,2]),]

# Plot the proportional count by Julian date:

  plot(prop.of.lists~jdays, data = prop.of.lists.df,
       xlab = 'Julian day',
       ylab = 'Proportion of lists')

# Fit a logistic curve to the data. 
# Note: Finding the optimal curve is outside of the
# realm of this course, if you would like to explore how to fit a curve, the function
# is included in the source file.

  add.log.curve()

# Calculate the inflection point of the curve. As above, this is found on the 
# source file for simplicity.

  calc.inflection()

#======================================================================*
# ---- Calculate the inflection point across years ----
#======================================================================*

# In the source file (for simplicity), there is a function that combines
# the above in order to create a vector of inflection points for each 
# year of data.

# Let's look again at the years in the study:

  head(inbu)
  
  sort(unique(inbu$Year))

# Create a vector of years:

  years = sort(unique(inbu$Year))

# Create an empty vector of inflection points:

  i.points = numeric()

# For loop to calculate the inflection point across years, this one
# will take a bit of time to run:

  for (i in 1:length(years)){
    i.points[i] = inflection.year(years[i])
  }

  i.points

# Make into a data frame, then remove NA values (unable to estimate):

df = data.frame(years, i.points)
df = df[!is.na(df$i.points),]

# Plot the data and look to see if there is a trend (exploration only):

  plot(i.points~years, data = df, 
       pch = 19,cex.lab = 1.3,
       xlab = 'Year', ylab = 'Inflection point')

# Add lines:

  lines(i.points~years, data = df, lwd = 2)

# Model the change in arrival date (inflection point):

  mod = lm(i.points~years)

# Add a line of best fit:
  
  abline(mod, lwd = 2, lty = 2)

# Summarize the model:

  summary(mod)

# Question: What is the decadal change in Indigo bunting arrival date
# for Front Royal, Virginia?

# Challenge: If we defined arrival date as the date of first arrival, what
# would be the observed trend?
  

