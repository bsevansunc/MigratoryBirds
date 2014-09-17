MigratoryBirds
==============


# Load packages and libraries

install.packages(Jags)
install.packages(rjags)
library(Jags)
library(R2jags)
library(rjags)

####################################################

#setwd and load data
setwd("C:/Users/User/Desktop/MODELING/")
spec2<-read.table("C:/Users/User/Desktop/MODELING/s2spec.csv",header=T,sep=",")
head(spec2)
dim(spec2)
veg2<-read.table("veginputs2.csv",header=T,sep=",")
head(veg2)
dim(veg2)
names(spec2)
names(veg2)
 
#################################################

# Old sequence of sites for 4 ocassions in resident model
# seq.site=rep(0,348)
# for (i in seq(4,348,4)){
#   seq.site[(i-3):i]=i/4
# }
#####################Make the model a function- Careful with the object POINT as it tends to be a problematic item

abun.calc=function(j){ 
  
  ##############Create sequence of sites############
  
  seq.site = rep(1:69, each = 2)
  # View(seq.site)
  
  ### Corrected abundance based on elevation, observer& habitat
  
  # Define model
  sink("BinMixspec.txt")
  cat("
      model {
      # Priors
      
      alpha.lam ~ dunif(-10,10)  #intercept for abundance for each species
      
      beta1.lam ~ dunif(-10,10) #slope of elevation in km
      
      beta2.lam ~ dunif(-10,10) #habitat type
      
      for (i in 1:n.obs){
      p[i] ~ dunif(0,1) #intercept for detection rate for each observer
      }
      
      
      
      # Likelihood
      # Biological model for true abundance of spec
      for (i in 1:R){                                        #loop over R sites
      
      N[i] ~ dpois(lambda[i])             
      log(lambda[i]) <- alpha.lam + beta1.lam * ELEV[i] + beta2.lam * HAB[i]
      
      }
      # Observation model for replicated counts
      for (i in 1:occ){                        #loop over number of observations
      
      # Loop over all number of species
      C[i] ~ dbinom(p[OBS[i]], N[POINT[i]]) #this is the binomial mixture model
      
      } 
      # Derived quantities
      
      totalN <- sum(N[])                                              # Estimate total population size accrosss all sites
      
      #     ###population size per point?
      # for (i in 1:R){
      #     totalN[i] <- sum(N[,i]) 
      
      }
      
      ",fill=TRUE)
  sink()
  
  #R is number of survey points
  #Occ is combined number of ocassions per point-or total number of visits accross all survey stations, 2 in this case
  ##POINT is sequence of sites created
  
  OBS=spec2$OBS
  
  # Bundle data
  R=nrow(veg2)
  occ=nrow(spec2)
  POINT=seq.site
  win.data <- list(R = R, occ=occ, C = j, OBS=spec2$OBS, POINT=seq.site, n.obs=2, ELEV=veg2$ELEVATIONkm, HAB=veg2$HABTYPE)
  
  
  list(R, occ, C,POINT)
  
  # Inits function for 4 ocassions
  # Nst <- rep(0,R)
  # for (i in seq(4,348,4)){
  #   Nst[i/4] <- max(spec2$spec[(i-3):i])
  # }
  
  Nst <- rep(0,R)
  for (i in seq(2,138,2)){
    Nst[i/2] <- max(j[(i-1):i])
  }
  
  inits <- function(){list(N = Nst, alpha.lam=rnorm(1,0,1), beta1.lam=rnorm(1, 0, 1), beta2.lam=rnorm(1, 0, 1), 
                           p=runif(2, 0, 1))}
  
  # Parameters to estimate
  params <- c("totalN","alpha.lam", "beta1.lam", "p", "beta2.lam")
  
  # MCMC settings (these are just for testing and ni should be higher when you really run it)
  nc <- 3
  nb <- 200
  ni <- 5000
  nt <- 10
  
  ##INCREASE ni, lowered it for trials!
  
  # out <- bugs(win.data, inits, params, "BinMix2spec.txt", n.chains=nc, n.iter=ni, 
  #n.burn = nb, n.thin=nt, debug = TRUE)
  
  spec.model<-jags.model('BinMixspec.txt',win.data,inits,n.chain=nc,n.adapt=100)
  out<-coda.samples(spec.model,params,n.iter=ni,thin=nt,n.burnin=nb)
  
  plot(out)
  traceplot(out)
  summary(out)
  print(summary(out))
  gelman.diag(out)
  gelman.plot(out)
  xyplot(out)
  densityplot(out)
  effectiveSize(out)
}

##Try out function using lower ni
abun.calc(spec2$CIPA)


######################################################## Species list for calling out function


spplist= list("CIPA", "CARP", "CICO", "BARR", "BOBO", "BOME", "CABO", "CARR", "CHCH", "CHIC", "CHIN", "CICA", "CIHI", "CRAC", "CUOJ", "GAGA", "GODO", "JUCH", "JUDI", "MANU", "MARO", "PACE", "PATU", "PAVA", "PECO", "PEJA", "PETI", "RABI", "ROLI", "ROTU", "RUIS", "TOAL", "ZUGR", "ZUSP", "ZUMB", "ZUES", "OVEN", "CAND", "CERU", "PEGA", "PRAD", "PARU", "RAYA", "TIGR","UNWA", "LOWA", "ENMAA")
#########################################################
####Loop over all species in dataframe spec2


allspec=(spec2[,32:78 ])

for (j in 1:length(allspec)){
  abun.calc(j)
  
}

#################Make it export all values

