#----------------------------------------------------------------------*
# ---- Source functions for R Intro script ----
#======================================================================*

#----------------------------------------------------------------------*
# ---- Source functions for Distance script ----
#======================================================================*

#----------------------------------------------------------------------*
# ---- Source functions for RMark script ----
#======================================================================*

#----------------------------------------------------------------------*
# ---- Source functions for Phenology script ---- 
#======================================================================*

prop.by.DayYear = function(day, year){
  # List subset and observations per location 
    lists = lists[lists$Year.Collected == year & lists$JulianDay == day,]
    if (dim(lists)[1] != 0){
      lists.df = data.frame(lists$Longitude, lists$Latitude)  
      lists.sp =  SpatialPoints(lists.df, 
                    proj4string = CRS('+proj=longlat +datum=WGS84'))
      lists.ras = rasterize(lists.sp,r, fun = 'count', background = 0)   
      } else {
      lists.ras = r
      values(lists.ras) <- 0
    }
  # inbu subset and observations per location
    inbu = inbu[inbu$Year == year & inbu$JulianDay == day,]
    if (dim(inbu)[1] != 0) {
    inbu.df = data.frame(inbu$Longitude, inbu$Latitude)
    inbu.sp =  SpatialPoints(inbu.df, proj4string = CRS('+proj=longlat +datum=WGS84'))
    inbu.ras = rasterize(inbu.sp,r, fun = 'count', background = 0)
    } else {
      inbu.ras = r
      values(inbu.ras) <- 0
    }
  # Return a raster of the proportion of lists that contain inbu:
    inbu.ras/lists.ras
  }

# Likelihood ratio function:

lr.negLogLike <- function(beta, y, x) {
  beta0 <- beta[1]
  beta1 <- beta[2]
  psi <- plogis(beta0 + beta1*x) # same as:
  #    psi <- exp(beta0 + beta1*x) / (1 + exp(beta0 + beta1*x))
  likelihood <- psi^y * (1-psi)^(1-y) # same as:
  #   likelihood <- dbinom(y, 1, psi)
  return(-sum(log(likelihood)))
}

# Plot logistic curve to phenology data:

add.log.curve = function(...){
  x = prop.of.lists.df[,1]
  y = prop.of.lists.df[,2]
  starting.values <- c(0,0)
  opt.out <- optim(starting.values, lr.negLogLike, y=y, x=x,
                   hessian=TRUE)
  beta1 = opt.out$par
  lines(x,plogis(beta1[1] + beta1[2]*x), lwd = 2)
}

# Calculate inflection point:

calc.inflection = function(){
  m = findiplist(cbind(prop.of.lists.df[,1]),cbind(prop.of.lists.df[,2]),1)
  m[1,3]
}

# Wrapper function calculates the inflection point for a given year
# in Front Royal, Virginia:

inflection.year = function(year, cell){
  prop.of.lists = numeric()
  for (i in 1:length(jdays)){
    prop.of.lists[i] = prop.CellDayYear(93,jdays[i],year)
  }
  prop.of.lists.df = na.omit(data.frame(jdays,prop.of.lists))
  calc.inflection()
}
