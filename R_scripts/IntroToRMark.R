#######################################################################*
# ---- MIGRATORY BIRD COURSE: RMARK LAB ---- 
#######################################################################*
# Title: Introduction to RMark
# Author: Brian Evans, modified from Laake 2009
# Date created: 22 Aug 2014
# Overview: This script takes students through the steps associated with
# developing a Cormack-Jolly-Seber survival model. Students will:
#   1) Explore the basics RMark input files
#   2) Learn to run simple and more complex models
#   3) Assess model support and use model averaging to obtain survival
#     and detectability estimates.

#======================================================================*
# ---- Set-up ----
#======================================================================*

# RMark is not a stand alone program but, rather, an R interface to Mark.
# Before we begin, you may have to install Program Mark on you computer.
# It is located here: http://www.phidot.org/software/mark/downloads/

# Reading much ofthe "Gentle Introduction ..." is necessary and is 
# available for download here: http://www.phidot.org/software/mark/docs/book/"
# Appendix C of this book contains >100 pages dedicated to RMark. Much
# of the material in this introduction is from Appendix C.

install.packages('RMark')
library('RMark')

# For Mac and Linux users, you must provide R with the path to the executable
# Mark file. On my computer, this is done as follows (not run):
# MarkPath='/Users/bsevans/Applications'


#======================================================================*
# ---- Exploring RMark basics ----
#======================================================================*
# We will use a dataset in the RMark package to learn the fundamentals
# of a mark-recapture file and explore the building blocks of RMark.

# Get data (European Dipper capture history):

  data(dipper)

# Explore the data:

  class(dipper)

  summary(dipper)

  str(dipper)

  head(dipper)

# The field "ch" stands for encounter (capture) history. Notice
# that each encounter history record contains a string of 
# 0's (no encounter) and 1's (capture or recapture). Preceding
# 1's are not analyzed (nor is the initial 1, directly).

# ---- Run the simplest of MARK models ----

# We'll now start right away by running a very simple MARK model, but
# we do that we need to take a precautionary step. When you run a MARK
# analysis, it generates a lot of files. Find your current working
# directory in R (getwd function) and in your file system create a 
# new folder called "mark_scratch". Reset your working directory as 
# follows:

setwd('mark_scratch')

# Run simple MARK model:

mark1 = mark(dipper)

# On the output you will see:
# The MARK model: Phi(~1)p(~1) (intercept-only model)
# Parameter count, Negative log-likelihood, and AIC (more on this later)
# Beta parameters (slopes), with standard error and confidence intervals
# Real parameter estimates.

# You can explore model output further using:

summary(mark1)

coef(mark1)

get.real(mark1, 'Phi')

get.real(mark1, 'p')

# Real estimates can be computed "by hand" by running the "plogis" 
# function on the beta estimate for a given parameter. Try taking 
# the plogis of each of the beta parameter estimates.

# To look at the underlying structure of the model, the Parameter
# Index Matrices (PIMs), you can use the PIMS function. Try simplified
# as both true and false and observe the output. We'll take a moment
# to discuss what this means.

PIMS(mark1, 'Phi', simplified = T)

PIMS(mark1, 'p', simplified = T)

# ---- A more complex MARK model ----

# We will now explore a dataset that was simulated for RMark.

example.data = convert.inp("dipper",group.df=data.frame(sex=c("Male","Female"))) 

data(example.data)

head(example.data)

str(example.data)

summary(example.data)

# Here, we see a much more complex data structure. In addition
# to capture history and sex, there is a numeric variable (weight)
# and two additional factor variables (age and region).

# To run this model, we will first make it ready for running in
# MARK. At this point, it is necessary to know that MARK views 
# continuous and categorical (nominal) variables differently.
# Categorical variables are known as group variables and must
# be specified accordingly.
# Continuous variables are known as covariates (which are simpler
# in model construction but provide challenges in assessment).

# First we will process the encounter history data frame. 

d.proc = process.data(example.data,
                      groups='sex'),
                      begin.time = 2000)

# Explore the process data:

class(d.proc)

names(d.proc)

# Next, we make design data dataframe for our analysis based
# on the parameter index matrix. This connects the individual
# sampled with the sampling and data structure.

d.ddl = make.design.data(d.proc, remove.unused = T)

# As above, take a moment ot explore the output of the design
# data.

# Now, we are ready to construct our models. We must name
# each model for each parameter, covariate, and group variable
# of interest. Be careful, however, these models can get cumbersome
# very quickly!

# One variable for Phi:

Phi.dot = (formula=~1)
Phi.sex = (formula=~sex)
Phi.weight = (formula=~weight)

# To make an additive model, simple use a "+" sign.
# Two variables for Phi, additive:

Phi.sex.weight = (formula=~sex+weight)

# For an interaction term, there are two ways to
# express it. Two variables for Phi, interaction:

Phi.sex.i.weight = (formula=~sex+weight+sex:weight)

# Alternatively ...

Phi.sex.i.weight = (formula=~sex*weight)

# Define models for p (we'll just use year and sex)

p.dot = (formula=~1)
p.sex = (formula=~sex)

# Now we collect all of the models into a single model list:

cml = create.model.list("CJS")

# Let's take a moment to explore the design structure of these 
# models (PIMS function) prior to running them.

# Then run the models all at once:

example.models = mark.wrapper(cml,data=d.proc,ddl=d.ddl, invisible = F)

# ---- Model selection ----

# For model selection, it is easy to create an AIC table. We will run
# the model table function and then discuss the results:

mod.tab = model.table(example.models, use.lnl = T)

mod.tab

# ---- Model averaging ----
# With the AIC weights from our model table, we can average models
# by AIC weight to calculate real parameter estimates for group
# variables.

model.average(example.models)

# ---- Covariate predictions ----
# Likewise, we can predict covariate values, but the process requires
# some care.

Phi.weight.predictions=covariate.predictions(example.models, 
            data=data.frame(index=rep(1,20),weight=1:20)) 

# And create a simple plot of the results:

with(Phi.weight.predictions$estimates, 
{ 
  plot(weight, estimate,type="l",lwd=2,xlab="Weight(g)", 
       ylab="Survival",ylim=c(0,1)) 
  lines(weight,lcl,lty=2) 
  lines(weight,ucl,lty=2) 
}) 







