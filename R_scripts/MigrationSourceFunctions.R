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
      lists.sp =  SpatialPoints(lists.df, proj4string = CRS('+proj=longlat +datum=WGS84'))
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

inflection.year = function(year){
  l1 = list()
  for (i in 1:length(jdays)){
    # Extract values as vectors in the list:
    l1[[i]] = as.data.frame(prop.by.DayYear(jdays[i], year))[,1]
  } 
  prop.of.lists = numeric()
  for (i in 1:length(l1)){
    prop.of.lists[i] = l1[[i]][105]
  }
  prop.of.lists.df = data.frame(jdays,prop.of.lists)
  prop.of.lists.df = prop.of.lists.df[!is.na(prop.of.lists.df[,2]),]
  findiplist(cbind(prop.of.lists.df[,1]),cbind(prop.of.lists.df[,2]),1)[1,3]
}

