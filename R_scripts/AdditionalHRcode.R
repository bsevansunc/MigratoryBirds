#Extra code for the home range lab, migratory birds course
#these are hidden so that the participants don't cheat!


#EXCERCISE: remove the column called "rec" from the locs dataset
#this way the columns will just be X,Y,Z, and bird
##***REMOVE THIS LINE
locs$rec<-NULL

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

#3D asymptote analysis on bird b
 #this takes a LONG time - perhaps an hour or more depending on settings
# Start the clock!
ptm <- proc.time()

set.seed(0)
Vol95.1 = matrix(0, nrow=10, ncol=nrow(b)-4) #nrow must be the number of samples - 100 would be ideal but will take forever

for (x in 60:nrow(b)) { #this step does the calculation from 60 till the length of the dataset
  #if you could run every few pts instead of every one it would be much faster
    #but haven't figured out how to do that yet.
  for (k in 1:nrow(Vol95.1)) {  
    rows <- sample.int(nrow(b), x) 
    Hb <- Hpi(b[rows,])
    fhatb <- kde(x=b[rows,], H=Hb, gridsize=151, binned=FALSE)
    Vol95.1[k,x-4] <- contourSizes(fhatb, cont=95)
  }
}
# Stop the clock
proc.time() - ptm