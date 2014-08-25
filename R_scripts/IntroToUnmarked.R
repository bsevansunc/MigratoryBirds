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
#   1) Explore poisson regression in unmarked to estimate abundance.
#   2) Explore logistic regression in unmarked to estimate
#     occurance probability.
#   3) Explore methodology for point count analysis using the removal
#   method.
#   4) Explore methodology for point count analysis using the distance
#   method.
# 

library(unmarked)

# =======================================================================*
# ---- PART ONE: MODELING ABUNDANCE (POISSON REGRESSION)----
# -----------------------------------------------------------------------*
#
# -----------------------------------------------------------------------*
# An introduction to estimating abundance using poisson regression in 
# unmarked using simulated data
# -----------------------------------------------------------------------*

# 1. Set-up the data (simulate it here)
#
# Create a covariate called vegHt
nSites = 100
set.seed(443)                # so that we all get the same values of vegHt
vegHt = runif(nSites, 1, 3) # uniform from 1 to 3

# Suppose that expected population size increases with vegHt
# The relationship is described by an intercept of -3 and
#    a slope parameter of 2 on the log scale

lambda = exp(-3 + 2*vegHt)

# Now we go to 100 sites and observe the # of individuals (perfectly)

N = rpois(nSites, lambda)

# 2. Fit some models

# We can fit a model that relates abundance to vegHt using the glm() function
#  with "family=Poisson":

fm.glm1 = glm(N ~ vegHt, family=poisson)


# 3.  Do some analysis of the results

plot(vegHt, N, xlab="Vegetation height", ylab="Abundance (N)")
glm1.est = coef(fm.glm1)
plot(function(x) exp(-3 + 2*x), 1, 3, add=TRUE, lwd=2)
plot(function(x) exp(glm1.est[1] + glm1.est[2]*x), 1, 3, add=TRUE,
     lwd=2, col="blue")
legend(1, 15.9, c("Truth", "Estimate"), col=c("black", "blue"), lty=1,
       lwd=2)

# ------------------------------------------------------------------------------*
# ------ Imperfect observation of abundance using a binomial sampling model
## ----- suppose y is a BINOMIAL sample based on population size N and parameter
## ----- p = "detection probability". i.e.,:
## -----   y ~ binomial(N, p)
## ----- the canonical "point counting" model
## ----- In practice, we think p < 1, say p = 0.6. This is INDIVIDUAL-LEVEL
## ------ detection -- i.e., each individual is detected with probability p
##
## ----- In this case, N is a LATENT VARIABLE (i.e., unobserved).
# ------------------------------------------------------------------------------*

nVisits = 4
p = 0.6
y = matrix(NA, nSites, nVisits)
for(i in 1:nSites) {
  y[i,] = rbinom(nVisits, N[i], p)
}

# Format for unmarked and summarize
library(unmarked)
umf = unmarkedFramePCount(y=y, siteCovs=as.data.frame(vegHt))
summary(umf)

# Fit a model and extract estimates
# Detection covariates follow first tilde, then come abundance covariates

fm.nmix1 = pcount(~1 ~vegHt, data=umf)

# Note the following warning message:
#> fm.nmix1 = pcount(~1 ~vegHt, data=umf)
#Warning message:
#In pcount(~1 ~ vegHt, data = umf) : K was not specified and was set to 116.
# K is upper limit of summation for calculating the likelihood

# Consider other abundance models: NB = negative binomial, ZIP = zero-inflated Poisson
# currently no others available. 

fm.nmix2=  pcount(~1 ~vegHt, data=umf,mixture="NB")
fm.nmix3=  pcount(~1 ~vegHt, data=umf,mixture="ZIP")
beta1 = coef(fm.nmix1) 


# Note, estimates of detection coefficients are on the logit-scale
# When there are no covariates, we can back-transform using:

exp(beta1[3]) / (1+exp(beta1[3]))   # or

plogis(beta1[3])                    # or

backTransform(fm.nmix1, type="det") # estimate with SE

# When covariates are present we can do something like:

plot(function(x) exp(beta1[1] + beta1[2]*x), 1, 3,
     xlab="vegetation height", ylab="Expected Abundance")

# Or suppose you want predictions for new values of vegHt, say 1.2 and 3.1
newdat = data.frame(vegHt=c(1.2, 3.1))
predict(fm.nmix1, type="state", newdata=newdat)

# -----------------------------------------------------------------------*
# ----------- Now let's start from scratch with real data --------------
# -----------------------------------------------------------------------*

# 1. Set-up the data for analysis
#
# -------------------------- Format data ---------------------------------
# This a subset of point-count data from Chandler et al. (Auk 2009)
# alfl is Alder Flycatcher (Empidonax alnorum)

# Import data and check structure
alfl.data = read.csv("alfl05.csv", row.names=1)

str(alfl.data)

# Pull out count matrix 
# No need to covert to binary as we did for occupancy model

alfl.y = alfl.data[,c("alfl1", "alfl2", "alfl3")]

# Standardize site-covariates
woody.mean = mean(alfl.data$woody)
woody.sd = sd(alfl.data$woody)
woody.z = (alfl.data$woody-woody.mean)/woody.sd

struct.mean = mean(alfl.data$struct)
struct.sd = sd(alfl.data$struct)
struct.z = (alfl.data$struct-struct.mean)/struct.sd

# Create unmarkedFrame

alfl.umf = unmarkedFramePCount(y=alfl.y,
            siteCovs=data.frame(woody=woody.z, struct=struct.z),
            obsCovs=list(time=alfl.data[,c("time.1", "time.2", "time.3")],
            date=alfl.data[,c("date.1", "date.2", "date.3")]))
summary(alfl.umf)

# Here's an easy way to standardize covariates after making the UMF
obsCovs(alfl.umf) = scale(obsCovs(alfl.umf))
summary(alfl.umf)

# -------------------------- Model fitting  -----------------------------

(fm1 =  pcount(~1 ~1, alfl.umf))
backTransform(fm1, type="state")
backTransform(fm1, type="det")

(fm2 = pcount(~date+time ~1, alfl.umf))
(fm3 = pcount(~date+time ~woody, alfl.umf))
(fm4 = pcount(~date+time ~woody+struct, alfl.umf))
(fm5 = pcount(~date+time ~1, alfl.umf,mixture="NB"))
(fm6 = pcount(~date+time ~1, alfl.umf,mixture="ZIP"))
(fm7 = pcount(~date+time ~woody,alfl.umf,mixture="ZIP"))
(fm8 = pcount(~date+time ~struct,alfl.umf,mixture="ZIP"))
(fm9 = pcount(~date+time ~woody+struct, alfl.umf,mixture="ZIP"))
(fm10= pcount(~date+time ~woody+struct, alfl.umf,mixture="NB"))

# -------------------------- Model selection -----------------------------

# Put the fitted models in a "fitList"
fms = fitList("lam(.)p(.)"                    = fm1,
               "lam(.)p(date+time)"            = fm2,
               "lam(woody)p(date+time)"        = fm3,
               "lam(woody+struct)p(date+time)" = fm4,
               "lam(.)p(date+time)NB"          = fm5,
               "lam(.)p(date+time)ZIP"         = fm6,
               "lam(woody)p(date+time)ZIP"     = fm7,
               "lam(struct)p(date+time)ZIP"    = fm8,
               "lam(woody+struct)p(date+time)ZIP"=fm9,
               "lam(woody+struct)p(date+time)NB" =fm10)

# Rank them by AIC
(ms = modSel(fms))

# Table with everything you could possibly need
coef(ms)
toExport = as(ms, "data.frame")

## 2. Do some analysis of the results

# ---------------------------- Prediction --------------------------------

# Expected detection probability as function of time of day
# We standardized "time", so we predict over range of values on that scale
# We must fix "date" at some arbitrary value (let's use the mean)

newData1 = data.frame(time=seq(-2.08, 1.86, by=0.1), date=0)
E.p = predict(fm4, type="det", newdata=newData1, appendData=TRUE)
head(E.p)

par(mfrow=c(2,1))

# Plot it
plot(Predicted ~ time, E.p, type="l", ylim=c(0,1),
     xlab="time of day (standardized)",
     ylab="Expected detection probability")
lines(lower ~ time, E.p, type="l", col=gray(0.5))
lines(upper ~ time, E.p, type="l", col=gray(0.5))

# Expected abundance over range of "woody"
newData2 = data.frame(woody=seq(-1.6, 2.38,,50),struct=seq(-1.8,3.2,,50))
E.N = predict(fm4, type="state", newdata=newData2, appendData=TRUE)
head(E.N)

# Plot predictions with 95% CI
plot(Predicted ~ woody, E.N, type="l", ylim=c(-.1,max(E.N$Predicted)),
     xlab="woody vegetation (standardized)",
     ylab="Expected abundance, E[N]")
lines(lower ~ woody, E.N, type="l", col=gray(0.5))
lines(upper ~ woody, E.N, type="l", col=gray(0.5))

par(mfrow=c(1,1))
# Plot it again, but this time convert the x-axis back to original scale
plot(Predicted ~ woody, E.N, type="l", ylim=c(-.1,max(E.N$Predicted)),
     xlab="Percent cover - woody vegetation",
     ylab="Expected abundance, E[N]",
     xaxt="n")
xticks = -1:2
xlabs = xticks*woody.sd + woody.mean
axis(1, at=xticks, labels=round(xlabs, 1))
lines(lower ~ woody, E.N, type="l", col=gray(0.5))
lines(upper ~ woody, E.N, type="l", col=gray(0.5))


# ---------------------------- Goodness-of-Fit analysis ----------------------------
### Here's an example of a bootstrap GoF analysis.
### Best model is in "fm4" object
###

# Function returning three fit-statistics.
fitstats = function(fm) {
  observed = getY(fm@data)
  expected = fitted(fm)
  resids = residuals(fm)
  sse = sum(resids^2)
  chisq = sum((observed - expected)^2 / expected)
  freeTuke = sum((sqrt(observed) - sqrt(expected))^2)
  out = c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}

(pb = parboot(fm4, fitstats, nsim=100, report=1))

### To look at bootstrap distributions do this
###plot(pb, main="")
print(pb)

## Now lets bootstrap a summary statistic 
## This is not too meaningful right now but we will do a similar thing later in 
## a more relevant context

# Total population size (derived parameter)
Nhat = function(fm) {
  N = sum(predict(fm, type="state")$Predicted, na.rm=TRUE)
}

(pb.N = parboot(fm4, Nhat, nsim=25, report=5))
plot(pb.N)

# =======================================================================*
# ---- PART TWO: MODELING OCCUPANCY PROBABILITY (LOGISTIC REGRESSION)----
# -----------------------------------------------------------------------*
#

# ------------------------- Simulate data ------------------------------

# Create a covariate called vegHt
nSites = 100
set.seed(443)                # so that we all get the same values of vegHt
vegHt = runif(nSites, 1, 3) # uniform from 1 to 3

# Suppose that occupancy probability increases with vegHt
# The relationship is described by an intercept of -3 and
#    a slope parameter of 2 on the logit scale
# plogis is the inverse-logit (constrains us back to the [0-1] scale)
psi = plogis(-3 + 2*vegHt)

# Now we go to 100 sites and observer presence or absence
# Actually, let's just simulate the data
z = rbinom(nSites, 1, psi)

# If detection probability is 1, we can estimate occurrence prob
#   using logistic regression
(fm.glm1 = glm(z ~ vegHt, family=binomial))

plot(vegHt, z, xlab="Vegetation height", ylab="Occurrence probability")
glm1.est = coef(fm.glm1)
plot(function(x) plogis(-3 + 2*x), 1, 3, add=TRUE, lwd=2)
plot(function(x) plogis(glm1.est[1] + glm1.est[2]*x), 1, 3, add=TRUE,
     lwd=2, col="blue")
legend(1, 0.9, c("Truth", "Estimate"), col=c("black", "blue"), lty=1,
       lwd=2)

# What if we don't want to use glm? Here is a more transparent approach:
# This is the negative log-likelihood.
lr.negLogLike = function(beta, y, x) {
  beta0 = beta[1]
  beta1 = beta[2]
  psi = plogis(beta0 + beta1*x) # same as:
  #    psi = exp(beta0 + beta1*x) / (1 + exp(beta0 + beta1*x))
  likelihood = psi^y * (1-psi)^(1-y) # same as:
  #   likelihood = dbinom(y, 1, psi)
  return(-sum(log(likelihood)))
}

# Look at (negative) log-likelihood for 2 parameter sets
lr.negLogLike(c(0,0), y=z, x=vegHt)
lr.negLogLike(c(-3,2), y=z, x=vegHt) # Lower is better!


# Let's minimize it
starting.values = c(0,0)
opt.out = optim(starting.values, lr.negLogLike, y=z, x=vegHt,
                 hessian=TRUE)

opt.out$par                        # MLEs are pretty close to glm1.est
sqrt(diag(solve(opt.out$hessian))) # close to SE in summary(fm.glm1)

# -----------------------------------------------------------------------*
# ------ Single-season occupancy model of MacKenzie et al. (2002) -------
# -----------------------------------------------------------------------*

# What if detection probability is less than one, say p=0.6?
# Now, z can no longer be observed, ie it is latent
# This simulates data under the MacKenzie model
# y_i ~ Bernoulli(z*p)
nVisits = 4
p = 0.6
y = matrix(NA, nSites, nVisits)
for(i in 1:nSites) {
  y[i,] = rbinom(nVisits, 1, z[i]*p)
}

# PART I. Format for unmarked and summarize
library(unmarked)
umf = unmarkedFrameOccu(y=y, siteCovs=as.data.frame(vegHt))
summary(umf)

# PART II. Fit a model. Note: estimates are on logit scale
# Detection covariates follow first tilde, then come occupancy covars
(fm.occu1 = occu(~1 ~vegHt, data=umf))

# PART III. Analyze results
# When there are no covariates, we can back-transform using:
(beta1 = coef(fm.occu1))
exp(beta1[3]) / (1+exp(beta1[3])) # or
plogis(beta1[3]) # or
backTransform(fm.occu1, type="det") # estimate with SE

# When covariates are present we can do something like
plot(function(x) plogis(beta1[1] + beta1[2]*x), 1, 3,
     xlab="vegetation height", ylab="Expected occupancy probability")

# Or suppose you want predictions for new values of vegHt, say 1.2 and 3.1
newdat = data.frame(vegHt=c(1.2, 3.1))
predict(fm.occu1, type="state", newdata=newdat)

# -----------------------------------------------------------------------*
# ----------- Now let's start from scratch with real data --------------
# -----------------------------------------------------------------------*

# -------------------------- PART I. Format data -------------------------
# This a subset of point-count data from Chandler et al. (Auk 2009)
# alfl is Alder Flycatcher (Empidonax alnorum)


# Import data from the website and check structure
alfl.data = read.csv("http://sites.google.com/site/unmarkedinfo/home/webinars/2012-january/data/alfl05.csv?attredirects=0&d=1", row.names=1)
str(alfl.data)

# Pull out count matrix and covert to binary
alfl.y = alfl.data[,c("alfl1", "alfl2", "alfl3")]
alfl.y1 = alfl.y # Make a copy
alfl.y1[alfl.y>1] = 1

# Standardize site-covariates
woody.mean = mean(alfl.data$woody)
woody.sd = sd(alfl.data$woody)
woody.z = (alfl.data$woody-woody.mean)/woody.sd

struct.mean = mean(alfl.data$struct)
struct.sd = sd(alfl.data$struct)
struct.z = (alfl.data$struct-struct.mean)/struct.sd

# Create unmarkedFrame
library(unmarked)
alfl.umf = unmarkedFrameOccu(y=alfl.y1,
     siteCovs=data.frame(woody=woody.z, struct=struct.z),
     obsCovs=list(time=alfl.data[,c("time.1", "time.2", "time.3")],
    date=alfl.data[,c("date.1", "date.2", "date.3")]))
summary(alfl.umf)

# Here's an easy way to standardize covariates after making the UMF
obsCovs(alfl.umf) = scale(obsCovs(alfl.umf))
summary(alfl.umf)

# -------------------- PART II. Model fitting ----------------------------

(fm1 = occu(~1 ~1, alfl.umf))
backTransform(fm1, type="state")
backTransform(fm1, type="det")

(fm2 = occu(~date+time ~1, alfl.umf))
(fm3 = occu(~date+time ~woody, alfl.umf))
(fm4 = occu(~date+time ~woody+struct, alfl.umf))
(fm5 = occu(~date+time+struct ~woody+struct, alfl.umf))

# ----------- PART III. Model selection, prediction, model-averaging ------

# Model selection

# Put the fitted models in a "fitList"
fms = fitList("psi(.)p(.)"                           = fm1,
               "psi(.)p(date+time)"                   = fm2,
               "psi(woody)p(date+time)"               = fm3,
               "psi(woody+struct)p(date+time)"        = fm4,
               "psi(woody+struct)p(date+time+struct)" = fm5)

# Rank them by AIC
(ms = modSel(fms))

# Do stuff
coef(ms)
toExport = as(ms, "data.frame")

# Prediction

# Expected detection probability as function of time of day
# We standardized "time", so we predict over range of values on that scale
# We must fix "date" at some arbitrary value (let's use the mean)
newData1 = data.frame(time=seq(-2.08, 1.86, by=0.1), date=0)
E.p = predict(fm3, type="det", newdata=newData1, appendData=TRUE)
head(E.p)

# Plot it
plot(Predicted ~ time, E.p, type="l", ylim=c(0,1),
     xlab="time of day (standardized)",
     ylab="Expected detection probability")
lines(lower ~ time, E.p, type="l", col=gray(0.5))
lines(upper ~ time, E.p, type="l", col=gray(0.5))

# Expected occupancy over range of "woody"
newData2 = data.frame(woody=seq(-1.6, 2.38, by=0.1))
E.psi = predict(fm3, type="state", newdata=newData2, appendData=TRUE)
head(E.psi)

# Plot predictions with 95% CI
plot(Predicted ~ woody, E.psi, type="l", ylim=c(0,1),
     xlab="woody vegetation (standardized)",
     ylab="Expected occupancy probability")
lines(lower ~ woody, E.psi, type="l", col=gray(0.5))
lines(upper ~ woody, E.psi, type="l", col=gray(0.5))

# Plot it again, but this time convert the x-axis back to original scale
plot(Predicted ~ woody, E.psi, type="l", ylim=c(0,1),
     xlab="Percent cover - woody vegetation",
     ylab="Expected occupancy probability",
     xaxt="n")
xticks = -1:2
xlabs = xticks*woody.sd + woody.mean
axis(1, at=xticks, labels=round(xlabs, 1))
lines(lower ~ woody, E.psi, type="l", col=gray(0.5))
lines(upper ~ woody, E.psi, type="l", col=gray(0.5))

# Model-averaging

# Average predictions from all models
# See pg 150, section 4.2.1, of Burnham and Anderson (2002)
# This might be worthwhile since fm3 and fm4 had similar support

newData3 = data.frame(woody=seq(-1.6, 2.38, by=0.1), struct=0)
E.psi.bar = predict(fms, type="state", newdata=newData3,
                     appendData=TRUE)
head(E.psi.bar)

# Plot it
plot(Predicted ~ woody, E.psi.bar, type="l", ylim=c(-0.1, 1.1),
     xlab="Percent cover - woody vegetation",
     ylab="Expected occupancy probability",
     xaxt="n")
xticks = -1:2
xlabs = xticks*woody.sd + woody.mean
axis(1, at=xticks, labels=round(xlabs, 1))
lines(lower ~ woody, E.psi.bar, type="l", col=gray(0.5))
lines(upper ~ woody, E.psi.bar, type="l", col=gray(0.5))

#----------------------------------------------------------------------*
# ---- PART 5: Removal method ----
#----------------------------------------------------------------------*
# We will start with the removal method. In this method, a point count
# is divided into fixed time intervals. After each time interval, an 
# individual is "removed" from the sample and subsequent observations
# count the remaining population of birds.

# The observations y[i] = (y[i,1],y[i,2],y[i,3]) of this dataset are 
# the numbers of ovenbird removed in each of three 5 minute intervals at
# point i. We assume that y[i] ~ multinomial(N[i], ...) and that
# N[i] ~ Poisson(lam[i]) or some other suitable distribution.

# 1. Load the data

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

# 2. We need to convert this to an "unmarked" data object, which is an S4 class:

ovenFrame = unmarkedFrameMPois(ovendata.list$data,
      siteCovs=as.data.frame(scale(ovendata.list$covariates[,-1])),
      type = "removal")

# See ?unmarkedFrameMPois for a description of this function and available 
# options.

# To see the data class of this object:

class(ovenFrame)

# Providing the name of this object will display it as if it were a data frame:

ovenFrame

# 3. Specify models for abundance (lambda) and detection probability estimates. The
# syntax for this is somewhat similar to that of RMark. Note, however that 
# there are two tildas. The first is for the lamda estimate and the
# second for detection probability.

fm0 = multinomPois(~1 ~1,ovenFrame)
fm1 = multinomPois(~ 1 ~ ufp + trba, ovenFrame)
fm2 = multinomPois(~ 1 ~ ufp + trba + ufp*trba, ovenFrame)
fm3 = multinomPois(~ ufp ~ ufp + trba, ovenFrame)
fm4 = multinomPois(~ ufp ~ ufp + trba +ufp*trba, ovenFrame)

# 4. Next, we create a list of models that we will use for model selection and/or
# model averaging:

ms= fitList( 
  "lam(.)p(.)"                                = fm0,
  "lam(ufp+trba)p(.)"                         = fm1,
  "lam(ufp+trba+ufp*trba)p(.)"                = fm2,
  "lam(ufp+trba)p(ufp)"                       = fm3,
  "lam(ufp+trba+ufp*trba)p(ufp)"              = fm4)

# Note the similarity in structure here between unmarked and RMark!

# We can quickly create an AIC table for model selection!

(ms1 = modSel(ms))

# And obtain parameter estimates!

coef(ms1)


ovendata.list
# note: every covariate is a "siteCovs" with a single primary sample, even if
# the covariate effects "p"

#----------------------------------------------------------------------*
# ---- PART 6: Distance method ----
#----------------------------------------------------------------------*
# Distance sampling analysis in unmarked

## 2 simulation examples

# Simulate line transect data
# Each transect is 100m long

nSites <- 50
strip.width <- 100 # half-width really
dist.breaks <- seq(0, strip.width, by=10)

sigma <- 30 # Scale parameter of half-normal detection function

# Half-normal detection function
g <- function(x, sig) exp(-x^2/(2*sig^2))
g(3, sig=sigma) # Detection probability at a distance of 30m

# Plot the detection function
curve(g(x, sig=20), from=0, to=100, xlab="Distance (x)",
      ylab="Detection probability")
curve(g(x, sig=50), from=0, to=100, add=TRUE, col="blue")


# Compute detection probability in each distance interval
p <- rep(NA, length(dist.breaks)-1)
for(j in 1:length(p)) {
  p[j] <- integrate(g, dist.breaks[j], dist.breaks[j+1],
                    sig=sigma)$value / 10 # 10 is width of the interval
}
round(p, 2)

# Compute the multinomial cell probs
psi <- rep(1/10, length(p)) # Probability of occurring in each interval
pi <- p * psi
sum(pi)
pi[length(p)+1] <- 1 - sum(pi)
round(pi, 2)

# Simulate data
lambda <- 5 # Expected number of indivdiuals in each plot
N <- rpois(nSites, lambda)
y <- matrix(NA, nSites, length(pi))
for(i in 1:nSites) {
  y[i,] <- rmultinom(1, N[i], pi)
}

head(y) # The last column is the number of individuals not detected!!!

y <- y[,-11]

library(unmarked)
umf <- unmarkedFrameDS(y=y, survey="line",
                       tlength=rep(100, nSites), # transect length
                       dist.breaks=dist.breaks, unitsIn="m")

fm <- distsamp(~1 ~1, data=umf, output="abund")
fm

exp(coef(fm)) # SHould be close to 5 and 30

# This creates "point transect" data
# This time we simulate continuous distance data

# Fake data
#set.seed(4550)
nSites <- 50
vegHt <- rnorm(nSites) # our favorite covariate
radius <- 100
distance.breaks <- seq(0, radius, by=20)

# Parameters
beta0 <- 0
beta1 <- 2
lambda <- exp(beta0 + beta1*vegHt)
sigma <- 50

# y=number of individuals detected in each distance interval
y <- matrix(0, nSites, length(distance.breaks)-1)
N <- rep(0, nSites)
for(i in 1:nSites) {
  M <- rpois(1, lambda[i]) # Number of individuals in the square
  if(M == 0)
    next
  # Coordinates of the M individuals in the square
  xy <- cbind(runif(M, -radius, radius),
              runif(M, -radius, radius)) # Origin = [0,0]
  # Distance from observer to the individual
  d <- sqrt((xy[,1] - 0)^2 + (xy[,2] - 0)^2) # Origin = [0,0]
  d <- d[d <= radius] # Discard individuals outside radius
  N[i] <- length(d)   # Number of individuals in the circle
  p <- g(x=d, sig=sigma) # Detection probability
  seen <- rbinom(N[i], 1, p) # Which individuals are detected
  if(all(seen == 0))
    next
  d1 <- d[seen==1] # The distance data for seen individuals
  counts <- table(cut(d1, distance.breaks, include.lowest=TRUE))
  y[i,] <- counts # The number of detections in each distance interval
}
colnames(y) <- names(counts)

head(y)

plot(xy, cex=1, pch=16, col="blue", ann=FALSE,
     xlim=c(-radius-5, radius+5), ylim=c(-radius-5, radius+5))
symbols(0, 0, circles=radius, add=TRUE, inches=FALSE)

library(unmarked)
umf <- unmarkedFrameDS(y=y, siteCovs=data.frame(vegHt=vegHt),
                       dist.breaks=distance.breaks,
                       survey="point", unitsIn="m")
summary(umf)

fm <- distsamp(~1 ~vegHt, umf, output="abund")
fm

re <- ranef(fm) # Note the warning
plot(re)[41:49] # Posterior is right truncated at site 42 -- problem

re <- ranef(fm, K=150)
plot(re)[41:49]

cbind(trueN=N, Nmode=bup(re, "mode"), confint(re))

### Analysis of Island Scrub-jay data. See the paper:
### Sillett, Chandler, Royle, Kery and Morrisson
### "Hierarchical distance sampling models
### to estimate global  population size and habitat-specific abundance of
### an island endemic"
### Ecological Applications 2012

### Requires unmarked version >=0.9-8 and "habgrid.R"

# --------------------- PART I: DATA FORMATTING --------------------------

library(unmarked)
data(issj)
str(issj)

## Using 100 m distance bands, define the limits of each band
db <- c(0, 100, 200, 300)

# In the paper we used gdistsamp() so that we could use NB distribution
# Here we will use distsamp() because its faster
umfFall <- unmarkedFrameDS(y = as.matrix(issj[,1:3]),
                           siteCovs=issj[,6:8], dist.breaks=db,
                           unitsIn="m", survey="point")

summary(umfFall)

# Now, let's standardize the covariates and add plot area
# We only need area b/c later we will predict to pixels of different area
sc <- siteCovs(umfFall)
sc.s <- scale(sc)
rm(pi)
siteCovs(umfFall) <- cbind(sc.s, area=pi*300^2/10000)
summary(umfFall)

# ------------------------ PART II: Fit models ---------------------------

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


# ---------- PART III: MODEL SELECTION, GOODNESS OF FIT, ETC... ----------

# Model selection
fitsFall <- fitList(fits=fall)
(msFall <- modSel(fitsFall))

fall$C2E.C

## GoF analysis

## define a fit statistic
freeTuke <- function(fm) {
  observed <- getY(fm@data)
  expected <- fitted(fm)
  sum((sqrt(observed) - sqrt(expected))^2)
}

# should set nsim=100 or more, but nsim=2 here for illustration
pbFall <- parboot(fall$C2E.C, freeTuke, nsim=50, report=5)

plot(pbFall) # Evidence of over-dispersion.
# This is why we used gdistsamp(*, mixture="NB") in the paper

# *Expected* population size for the sample points
getN <- function(fm, newdata=NULL)
  sum(predict(fm, type="state", newdata=newdata)[,1])

getN(fall$C2E.C)

# This does the same thing, as:
X <- model.matrix(~chaparral+I(chaparral^2)+elevation+log(offset(area)),
                  siteCovs(umfFall))
head(X) # The design matrix
sum(exp(X %*% c(coef(fall$C2E.C, type="state"), 1)))

# Empirical Bayes estimates of posterior distribution:
# Pr(N=x | y, lambda, sigma) for x=0,1,...,K
re.jay <- ranef(fall$C2E.C, K=100)

devAskNewPage(TRUE)
plot(re.jay, xlim=c(-1, 20), layout=c(3,3), subset=site %in% 250:276)

# *Realized* population size
sum(bup(re.jay, "mode")) # Why are this so different from predict?
# Because the Poisson assumption isn't very good

# Covariate effects

summary(umfFall) # Note the range of chaparral

newdat <- data.frame(chaparral = seq(-1.16, 2.9, length=100),
                     elevation = 0, area=28.27)

# Expected values of N for covariate values in "newdat"
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

# Total population size:
getN(fall$C2E.C, newdata=cruz.s)

# Parametric bootstrap for CI (nsims should be much higher)
# A much faster function could be written to doing the sum
set.seed(9387)
EN.B <- parboot(fall$C2E.C, stat=getN, nsim=20, report=1)
EN.B # 95% CI = (658, 1018)

# Let's map the predictions. ie create a species distribution map
# One way to do this is to rasterize the predictions (EN)
# Or, we could rasterize the covariates and use them in predict()

library(raster)
cruz.raster <- stack(rasterFromXYZ(cruz.s[,c("x","y","elevation")]),
                     rasterFromXYZ(cruz.s[,c("x","y","chaparral")]),
                     rasterFromXYZ(cruz.s[,c("x","y","area")]))
layerNames(cruz.raster) # These should match the names in the formula
plot(cruz.raster)

EN.raster <- predict(fall$C2E.C, type="state", newdata=cruz.raster)
plot(EN.raster)