
#====================================================================*
#----Setup Challenge----
#====================================================================*

#We learned lots of basic R skills yesterday now lets put those to good use. Recall yesterday that sex had both lower case and uppercase values for male, female and unknown. First lets change all those factor values to uppercase. Use the base command shortcut

NNW_data2$sex <- toupper(NNW_data2$sex)

#Second, lets do some data subsetting to get all records in the data frame where sex is not equalt to "u' or "U". Keep in mind that there is more than one way to do this. We can also use a logical statement to query multiple values for sex. Note: because we are going to want to work with the modified dataframe make sure you assign a new name.

NNW_data2 <- NNW_data[NNW_data$band_age!="U" & NNW_data$band_age!="u",]

NNW_data2 <- NNW_data[NNW_data$sex == 'M' | NNW_data$sex == "F",]

#Now lets get rid of a column that we are not going to use in the graphing excercise. Use your matrix skills to remove the tail column from the data frame.

NNW_data2 <- NNW_data2[,-5]

#====================================================================*
#----Histograms Challenge----
#====================================================================*

#Challenge now see if you can make a histogram of a different morphological variable for just one species by subsetting the data. Any ideas about how to make the bars colored?

NNW_AMRO <- NNW_data2[NNW_data2$species == "AMRO",]
head(NNW_AMRO)

ggplot(NNW_AMRO, aes(x=tars))+geom_histogram()

#====================================================================*
#----Summarizing Data Using Plyr Challenge----
#====================================================================*

#Challenge can you create a for loop that calculates the mean +/- SD  and then plot those lines on your facet graph?

sp_summ$mpsd <- 0
sp_summ$mmsd <- 0
for (i in 1:8){
  sp_summ$mpsd[i] <- sp_summ$mean_wg[i]+sp_summ$sd_wing[i]
  sp_summ$mmsd[i] <- sp_summ$mean_wg[i]-sp_summ$sd_wing[i]
}

sp_summ

#There is a much simpler way to do this can you figure it out? Think matrix notation.

sp_summ$mpsd[] <- sp_summ[,3]+sp_summ[,4]
sp_summ$mmsd[] <- sp_summ[,3]-sp_summ[,4]

sp_summ

ggplot(NNW_data2, aes(x=wing, fill=species))+geom_histogram(binwidth=1)+facet_grid(species~., scales="free")+scale_x_continuous("wing chord/mm", breaks=c(30,40,50,60,70,80,90,100,110,120,130,140), labels=c(30,40,50,60,70,80,90,100,110,120,130,140),limits=c(30,140))+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))+theme_bw()+ggtitle("Histograms of Wing Chord by Species")+geom_vline(data=sp_summ, aes(xintercept=mean_wg))+geom_vline(data=sp_summ, aes(xintercept=mpsd), linetype="dashed")+geom_vline(data=sp_summ, aes(xintercept=mmsd), linetype="dashed")

#====================================================================*
#----Bar Graphs Challenge----
#====================================================================*

#Challenge see if you use skills from yesterday to make a new column that indentifies species as resident or migrant and then make a facet grid graph using your new factor.

sp_summ$mig_stat <- NA
sp_summ
sp_summ$mig_stat <- ifelse(sp_summ$species == "CACH" |sp_summ$species == "NOCA"|sp_summ$species == "NOMO","resident", "migrant")

ggplot(sp_summ, aes(x=species, y=mean_wg, fill=species))+geom_bar(stat="identity")+facet_grid(mig_stat~.)+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))+theme(legend.position="none")+geom_errorbar(aes(ymin=mean_wg-sd_wing, ymax=mean_wg+sd_wing), width=0.1)+scale_y_continuous("Average Wing Chord (mm)")

#====================================================================*
#----More Advanced Data Summaries with Plyr Challenge----
#====================================================================*

#Challenge now use matrix notation to subset the dataframe

NNW_GRCA <- NNW_data2[NNW_data2$species == "GRCA",]

#Challenge can you guys write the code to summarize sample size, mean wing and se of wing by age and sex using ddply?

GRCADC_summ <- ddply(GRCA_DC2, c("SEX", "AGE"), summarise, n=length(BAND.NUM), mean_wg=mean(WG), sd_wg=sd(WG), se_wg=sd(WG)/sqrt(length(BAND.NUM)),mean_wt=mean(WT))

GRCADC_summ

GRCADC_summ2 <- ddply(GRCA_DC2, c("SEX"), summarise, mean_wg=mean(WG))

GRCADC_summ2

#====================================================================*
#----Scatterplots and Errorbars Challenge----
#====================================================================*

#Now try adjusting the color of the points, modify the ticks and change the theme

#Challenge try and change the text in some way (color, size, position, etc.)

#====================================================================*
#----Multiplot Function Challenge----
#====================================================================*

#Now see if you can add an a, b, c and d to your figures using geom_text