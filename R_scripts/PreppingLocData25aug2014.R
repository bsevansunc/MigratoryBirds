#Experiment 10 birds #these last five are temporary removals, where "post" is day 1 (w/o YW) and "pre" is day 3 (w/ YEWA)
yw10<-subset(locs.olap,Bird=="YWFA,Y_G" | Bird=="YWMO_Y_A", select=c("X","Y", "Z","Bird.Removal")) 
rs10a.pre<-subset(locs.olap,Bird.Removal=="RSFA_BK,O.pre", select=c("X","Y", "Z","Bird.Removal")) 
rs10b.pre<-subset(locs.olap,Bird.Removal=="RSMBK,BK_A.pre", select=c("X","Y", "Z","Bird.Removal")) 

ten<-rbind(yw10,rs10a.pre, rs10b.pre)
table(ten$Bird.Removal)->table
table

names(ten)[names(ten)=="Bird.Removal"]<-"bird"

ten$bird<-gsub(".pre", "", ten$bird)


#setting the working directory; 
setwd("/Users/LLPmac/Documents/AMRE_YEWA/MigratoryBirdsCourse/")

write.csv(ten, "HomeRangeData.csv")
#this CSV export doesn't do that weird shift thing.  


#and don't forget to set WD first

