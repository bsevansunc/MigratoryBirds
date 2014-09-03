#######################################################################*
# ---- INTRODUCTION TO THE PACKAGE UNMARKED: POINT COUNT LAB ---- 
#######################################################################*
# Title: Analysis of point count distance sampling data using the R
# package unmarked.
# Author: Scripts are modified from Chandler et al. 2012
# Date created: 22 Aug 2014
# Overview: This script takes students through the steps associated 
# with the analysis of avian point count data collected using removal
# and distance sampling methodologies. Students will:
#   1) Explore methodology for point count analysis using the removal
#   method.
#   2) Explore methodology for point count analysis using the distance
#   method.
#   Note: You have a script available to you that calculates abundance
#   and occupancy probabilities. We will not have time to run through
#   these here, but we can help you explore these outside of this lab.
  
  install.packages('unmarked','raster')
  library(unmarked)
  library(raster)

#----------------------------------------------------------------------*
# ---- PART 1: Removal method ----
#----------------------------------------------------------------------*
# We will start with the removal method. In this method, a point count
# is divided into fixed time intervals. After each time interval, an 
# individual is "removed" from the sample and subsequent observations
# count the remaining population of birds.

# The observations y[i] = (y[i,1],y[i,2],y[i,3]) of this dataset are 
# the numbers of ovenbird removed in each of three 5 minute intervals at
# point i. We assume that y[i] ~ multinomial(N[i], ...) and that
# N[i] ~ Poisson(lam[i]) or some other suitable distribution.

# (1) Load the data

  data(ovendata)

# Let's take a moment to explore the data, which is a list of two 
# separate data objects called "ovendata.list".

# How many objects does the list contain?

  length(ovendata.list)

# Let's look at the first list element:
  
  class(ovendata.list[[1]])
  
  ovendata.list[[1]]

# This is a matrix of observations of Ovenbird across time intervals
# (columns) for a given site (rows).

# Next, the second element:
  
  class(ovendata.list[[2]])
  
  ovendata.list[[2]]

# The is a data frame of site-level covariates with the site name [,1], and 
# two covariates.

# (2) Convert this to an "unmarked" data object, which is an S4 class:
  
  ovenFrame = unmarkedFrameMPois(ovendata.list$data,
        siteCovs=as.data.frame(scale(ovendata.list$covariates[,-1])),
        type = "removal")

# See ?unmarkedFrameMPois for a description of this function and available 
# options.

# To see the data class of this object:
  
  class(ovenFrame)

# Providing the name of this object will display it as if it were a data frame:
  
  ovenFrame

# (3) Specify models for abundance (lambda) and detection probability estimates. The
# syntax for this is somewhat similar to that of RMark. Note, however that 
# there are two tildas. The first is for the lamda estimate and the
# second for detection probability.
  
  fm0 = multinomPois(~1 ~1,ovenFrame)
  fm1 = multinomPois(~ 1 ~ ufp + trba, ovenFrame)
  fm2 = multinomPois(~ 1 ~ ufp + trba + ufp*trba, ovenFrame)
  fm3 = multinomPois(~ ufp ~ ufp + trba, ovenFrame)
  fm4 = multinomPois(~ ufp ~ ufp + trba +ufp*trba, ovenFrame)

# (4) Next, we create a list of models that we will use for model selection 
# and/or model averaging:
  
  ms= fitList( 
    "lam(.)p(.)"                                = fm0,
    "lam(ufp+trba)p(.)"                         = fm1,
    "lam(ufp+trba+ufp*trba)p(.)"                = fm2,
    "lam(ufp+trba)p(ufp)"                       = fm3,
    "lam(ufp+trba+ufp*trba)p(ufp)"              = fm4)

# We can quickly create an AIC table for model selection!

  (ms1 = modSel(ms))

# And obtain parameter estimates!
  
  coef(ms1)
  
  ovendata.list

# Note: every covariate is a "siteCovs" with a single primary sample, even if
# the covariate effects "p"

#----------------------------------------------------------------------*
# ---- PART 2: Distance method ----
#----------------------------------------------------------------------*
# Analysis of Island Scrub-jay data. See the paper: Sillett, Chandler, 
# Royle, Kery and Morrisson "Hierarchical distance sampling models
# to estimate global  population size and habitat-specific abundance of
# an island endemic". Ecological Applications 2012.

# ---- (1) DATA FORMATTING ----

# Load data:

  data(issj)

# Explore data:
  
  str(issj)
  
  summary(issj)

# Define the limits of each distance bands using 100 m increments:

  db <- c(0, 100, 200, 300)

# Create unmarked frame: 
  
  umfFall <- unmarkedFrameDS(y = as.matrix(issj[,1:3]),
                  siteCovs=issj[,6:8], dist.breaks=db,
                  unitsIn="m", survey="point")
  
  summary(umfFall)

# Now, let's standardize the covariates and add plot area. We only need 
# area b/c later we will predict to pixels of different area:
  
  sc <- siteCovs(umfFall)
  sc.s <- scale(sc)
    
  siteCovs(umfFall) <- cbind(sc.s, area=pi*300^2/10000)
  summary(umfFall)

# ---- (2) Fit models ----

  fall <- list()   # make a list to store the models

# These two are the same:
  fall$Null <- distsamp(~1 ~offset(log(area)), umfFall, output="abund")
  fall$Null_D <- distsamp(~1 ~1, umfFall, output="density")

# We use the offset instead of output="density" because later we
# will make predictions to pixels of different area
  
  hist(fall$Null, ylim=c(0,0.006)) # Q: why does it look like this?
  
  fall$Chap. <- distsamp(~1 ~chaparral + offset(log(area)), umfFall,
                         output="abund")
  fall$Chap2. <- distsamp(~1 ~chaparral+I(chaparral^2)+offset(log(area)),
                          umfFall, output="abund")
  fall$Elev. <- distsamp(~1 ~ elevation+offset(log(area)), umfFall,
                         output="abund")
  fall$Elev2. <- distsamp(~1 ~ elevation+I(elevation^2)+offset(log(area)),
                          umfFall, output="abund")
  fall$Forest. <- distsamp(~1 ~forest+offset(log(area)), umfFall,
                           output="abund")
  fall$Forest2. <- distsamp(~1 ~forest+I(forest^2)+offset(log(area)),
                            umfFall, output="abund")
  fall$.Forest <- distsamp(~forest ~offset(log(area)), umfFall,
                           output="abund")
  fall$.Chap <- distsamp(~chaparral ~offset(log(area)), umfFall,
                         output="abund")
  fall$C2E. <- distsamp(~1 ~ chaparral + I(chaparral^2) + elevation +
                          offset(log(area)), umfFall, output="abund")
  fall$C2F2. <- distsamp(~1 ~chaparral + I(chaparral^2) + forest +
                           I(forest^2)+offset(log(area)), umfFall,  output="abund")
  fall$C2E.F <- distsamp(~forest ~chaparral+I(chaparral^2)+elevation+
                           offset(log(area)), umfFall, output="abund")
  fall$C2E.C <- distsamp(~chaparral ~chaparral + I(chaparral^2) + elevation +
                           offset(log(area)), umfFall, output="abund")

# ---- (3) MODEL SELECTION ----

# Model selection:
  
  fitsFall <- fitList(fits=fall)
  (msFall <- modSel(fitsFall))

  fall$C2E.C

# Covariate effects
  
  summary(umfFall) 
  
  newdat <- data.frame(chaparral = seq(-1.16, 2.9, length=100),
                       elevation = 0, area=28.27)

# Expected values of N for covariate values in "newdat":
  
  E.N <- predict(fall$C2E.C, type="state", newdata=newdat, appendData=TRUE)
  
  head(E.N)

# Transform chaparral back to original scale

  chap.mean <- mean(issj$chaparral)
  chap.sd <- sd(issj$chaparral)
  
  E.N$chaparralO <- (E.N$chaparral * chap.sd) + chap.mean
  head(E.N)

# Relationship between chaparral and expected abundance
  plot(Predicted ~ chaparralO, E.N, type="l", lwd=2, log="y",
       xlab = "Chaparral cover",
       ylab = "Expected abundance",
       cex.lab=1.3,
       ylim=c(0.1, 20))
    lines(lower ~ chaparralO, E.N)
    lines(upper ~ chaparralO, E.N)

# Now, let's get predictions for the entire island
  
  data(cruz)
  str(cruz)

# Since we standardized the covariates, we need to standardize these too
# Note that we use the original mean and SD
  
  attributes(sc.s) # means are "scaled:center". SDs are "scaled:scale"
  
  cruz.s <- cruz
  cruz.s$elevation <- (cruz$elevation-202)/125
  cruz.s$chaparral <- (cruz$chaparral-0.270)/0.234
  cruz.s$area <- (300*300)/10000 # The grid cells are 300x300m=9ha

# Now, we can predict for each pixel in the landscape

  EN <- predict(fall$C2E.C, type="state", newdata=cruz.s)

# *Expected* population size for the sample points:
  
  getN <- function(fm, newdata=NULL)
    sum(predict(fm, type="state", newdata=newdata)[,1])

# Total population size:

  getN(fall$C2E.C, newdata=cruz.s)

# Let's map the predictions. ie create a species distribution map
# One way to do this is to rasterize the predictions (EN)
# Or, we could rasterize the covariates and use them in predict()
  
  cruz.raster <- stack(rasterFromXYZ(cruz.s[,c("x","y","elevation")]),
                       rasterFromXYZ(cruz.s[,c("x","y","chaparral")]),
                       rasterFromXYZ(cruz.s[,c("x","y","area")]))
  plot(cruz.raster)

# Plot one layer only and add a title:
  
  names(cruz.raster)
  
  plot(cruz.raster[[1]], main = 'Elevation')

# Try the above using the land cover layer "chaparral"!
  
  EN.raster <- predict(fall$C2E.C, type="state", newdata=cruz.raster)
  plot(EN.raster)

# As when we plotted single land cover layers above, try plotting
# just the prediction raster (note: start with the "names" function)!