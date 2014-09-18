### Clear memory console
rm(list=ls())

### Load required packages
  require(ggplot2)
  require(maps)
  require(gridExtra)

###------------------------------------- Set Parameters ----------------------------------------- 

  feather.intercept <- -17.57 # feather discrimination intercept
  feather.slope <- 0.95# feather disrimiation slope
  sy.discrim <- 0 # expected difference between HY and AHY feathers
  odds <- 4 # set odds ratio for setting likelihoods to 0 or 1 (< 1/odds = 0, > = 1)
  sddd <-  12 # Within-site variance in d2H

###------------------------------------- Create Basemap --------------------------------------------

  setwd("~/Desktop/Clark Saved/Presentations/Stable isotope lecture/Data")

### Isotope map
  basemap <- read.delim("Deuterium_XYs.txt") # read isotope data
  names(basemap) <- c("grid_code", "d2h", "x","y") # rename columns
  basemap$df.asy <- feather.intercept + feather.slope*basemap$d2h # correct precip values for feather discrim.
  basemap$df.sy <- basemap$df.asy - sy.discrim # correct for age-specific discrimination
  
  str(basemap)
  head(basemap)
  summary(basemap)

### Plot basemap
  d2h.precip <- ggplot(basemap, aes(x=x, y=y))+geom_raster(aes(fill=d2h)) + # Plot
                scale_fill_gradient(low = "red", high = "green",
                                    limits = c(min(basemap$df.asy),max(basemap$d2h)))
  













  d2h.asy <- ggplot(basemap, aes(x=x, y=y))+geom_raster(aes(fill=df.asy)) +
              scale_fill_gradient(low = "red", high = "green",
                                  limits = c(min(basemap$df.asy),max(basemap$d2h)))# Plot

  grid.arrange(d2h.precip, d2h.asy, nrow=2)

## ------------------------------------------ Import MOSI data ----------------------------------------------

  mosi <- read.csv("mosi.csv")
  mosi$age.x  <- as.factor(mosi$age.x)
  str(mosi)
  head(mosi)
  summary(mosi)

##--------------------------------------- Isotope Only Assignment -------------------------------------------

  assign <- list()
  
  for(i in 1:nrow(mosi)){
    prob <- numeric()
    for(j in 1:nrow(basemap)){  
      if (mosi$age.x[i] == 1)
        prob[j] <- dnorm(mosi$dd[i], mean = basemap$df.asy[j], sddd)
      else 
        prob[j] <- dnorm(mosi$dd[i], mean = basemap$df.sy[j], sddd) 
    }
    rel.prob <- prob/max(prob)
    occ <- ifelse(rel.prob < 1/odds, 0,1)
    assign[[i]] <- data.frame(x = basemap$x, y = basemap$y, z = rel.prob, o = occ)
  }

  str(assign[[1]])
  summary(assign[[1]])  
  
## ----------------------------------------- Plots --------------------------------------------

## Individual Plot ---- 
all_states <- map_data("state") 

indv.plot <- ggplot(assign[[3]], aes(x = x, y = y, fill = o)) + geom_raster() + coord_equal() + 
                scale_fill_gradient(low = "#FFEDA0", high = "#F03B20")+
                geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill =NA)+
                ggtitle("Individual 3\nLikelihood") + 
                theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank())
indv.plot

## Regional Plots ----
# North Region
  n <- which(mosi$region == "North") 
  north.assign <- assign[n]
  north.occ <- north.assign[[1]][,4]
  for(i in 2:length(north.assign)){
    north.occ <- north.occ + north.assign[[i]][,4]
  }
  north.df <- data.frame(x=basemap$x, y=basemap$y,z=north.occ)
  north.map <- ggplot(north.df, aes(x=x, y=y)) + geom_raster(aes(fill = z)) +
               geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill =NA)+
               scale_fill_gradient(low = "#FFEDA0", high = "#F03B20")+
               ggtitle("North") 


## Central Region
  c <- which(mosi$region == "Central")
  central.assign <- assign[c]
  central.occ <- central.assign[[1]][,4]
  for(i in 2:length(central.assign)){
    central.occ <- central.occ + central.assign[[i]][,4]
  }
  central.df <- data.frame(x=basemap$x, y=basemap$y,z=central.occ)
  central.map <- ggplot(central.df, aes(x=x, y=y)) + geom_raster(aes(fill = z)) +
                  geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill =NA)+
                  scale_fill_gradient(low = "#FFEDA0", high = "#F03B20")+
                  ggtitle("Central") 

# South Region
  s <- which(mosi$region == "South")
  south.assign <- assign[s]
  south.occ <- south.assign[[1]][,4]
  for(i in 2:length(south.assign)){
    south.occ <- south.occ + south.assign[[i]][,4]
  }
  south.df <- data.frame(x=basemap$x, y=basemap$y,z=south.occ)
  south.map <- ggplot(south.df, aes(x=x, y=y)) + geom_raster(aes(fill = z)) +
                  geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="black", fill =NA)+
                  scale_fill_gradient(low = "#FFEDA0", high = "#F03B20")+
                  ggtitle("South") 

  grid.arrange(north.map, central.map, south.map, nrow=2)

