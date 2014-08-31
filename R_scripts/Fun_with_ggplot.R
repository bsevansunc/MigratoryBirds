######################################################################
# MIGRATORY BIRDS COURSE
#####################################################################
#Author: Brandt Ryder
#Title: The Basics of Graphing in ggplot2
#Date Created: 20th August 2014
# Overview: This module will teach you the basics of graphing using the flexible ggplot package. While the base package of R provides many graphic functions, ggplot is data visualization package thats growing in popularity and uses key insights from Wilkison's Grammar of Graphics. By the end of the lab you should understand the basics of graph aesthetics and how to build graph layers to acheive print quality graphs. We will cover a range of basic and advancing graphing topics ranging from data summary tools to the creation of various kinds of graphs.

#====================================================================*
#----Setup----
#====================================================================*

#Install packages and load libraries

install.packages('ggplot2')

install.packages('plyr')

update.packages('ggplot2')

update.packages('plyr')

library(ggplot2)

library(plyr)

#Read in dataframe

NNW_data <- read.csv("Data/encounters.csv", header=TRUE)

#We learned lots of basic R skills yesterday now lets put those to good use. Recall yesterday that sex had both lower case and uppercase values for male, female and unknown. First lets change all those factor values to uppercase. Use the base command shortcut



#Second, lets do some data subsetting to get all records in the data frame where sex is not equalt to "u' or "U". Keep in mind that there is more than one way to do this. We can also use a logical statement to query multiple values for sex. Note: because we are going to want to work with the modified dataframe make sure you assign a new name.



#Now lets get rid of a column that we are not going to use in the graphing excercise. Use your matrix skills to remove the tail column from the data frame.



#Check header, data structure and summary info

head(NNW_data2)

str(NNW_data2)

str(NNW_data2)

summary(NNW_data2)

table(NNW_data$species)

#====================================================================*
#----Histograms----
#====================================================================*

#Creating histograms to visualize data

ggplot(NNW_data2, aes(x=wing))+geom_histogram(binwidth=1)

#Making Larger Bins

ggplot(NNW_data2, aes(x=wing))+geom_histogram(binwidth=2)

#Coloring by Species

ggplot(NNW_data2, aes(x=wing, fill=species))+geom_histogram(binwidth=1)

#Challenge now see if you can make a histogram of a different morphological variable for just one species by subsetting the data. Any ideas about how to make the bars colored?


#====================================================================*
#----Facets----
#====================================================================*

#Facet by Species

ggplot(NNW_data2, aes(x=wing, fill=species))+geom_histogram(binwidth=1)+facet_grid(species~.)

#Allowing the scales to vary by species

ggplot(NNW_data2, aes(x=wing, fill=species))+geom_histogram(binwidth=1)+facet_grid(species~., scales="free")

#Modifying ticks, labels, limits and axis title

ggplot(NNW_data2, aes(x=wing, fill=species))+geom_histogram(binwidth=1)+facet_grid(species~., scales="free")+scale_x_continuous("wing chord (mm)", breaks=c(30,40,50,60,70,80,90,100,110,120,130,140), labels=c(30,40,50,60,70,80,90,100,110,120,130,140),limits=c(30,140))

#Other approaches to just modify axis labels

ggplot(NNW_data2, aes(x=wing, fill=species))+geom_histogram(binwidth=1)+facet_grid(species~., scales="free")+xlab("Wing chord (mm)")+ylab("Count")

#====================================================================*
#----Colors and Themes----
#====================================================================*

#Mofidying colors
ggplot(NNW_data2, aes(x=wing, fill=species))+geom_histogram(binwidth=1)+facet_grid(species~., scales="free")+scale_x_continuous("wing chord/mm", breaks=c(30,40,50,60,70,80,90,100,110,120,130,140), labels=c(30,40,50,60,70,80,90,100,110,120,130,140),limits=c(30,140))+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))

#Changing the base theme
ggplot(NNW_data2, aes(x=wing, fill=species))+geom_histogram(binwidth=1)+facet_grid(species~., scales="free")+scale_x_continuous("wing chord/mm", breaks=c(30,40,50,60,70,80,90,100,110,120,130,140), labels=c(30,40,50,60,70,80,90,100,110,120,130,140),limits=c(30,140))+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))+theme_bw()

#Modifying elements of the theme
ggplot(NNW_data2, aes(x=wing, fill=species))+geom_histogram(binwidth=1)+facet_grid(species~., scales="free")+scale_x_continuous("wing chord/mm", breaks=c(30,40,50,60,70,80,90,100,110,120,130,140), labels=c(30,40,50,60,70,80,90,100,110,120,130,140),limits=c(30,140))+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))+theme(legend.position="none")+theme(axis.text.x=element_text(size=9, color="black"), axis.text.y=element_text(color="black"))+theme(axis.line = element_line(colour = "black"), axis.ticks=element_line(color="black"),                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       panel.grid.major = element_blank(),                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             panel.grid.minor = element_blank())                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  

#Adding a title
ggplot(NNW_data2, aes(x=wing, fill=species))+geom_histogram(binwidth=1)+facet_grid(species~., scales="free")+scale_x_continuous("wing chord/mm", breaks=c(30,40,50,60,70,80,90,100,110,120,130,140), labels=c(30,40,50,60,70,80,90,100,110,120,130,140),limits=c(30,140))+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))+theme_bw()+ggtitle("Histograms of Wing Chord by Species")

#====================================================================*
#----Summarizing Data Using Plyr----
#====================================================================*

#Summarizing data using the plyr package
#Here we want to use summary data for plotting means and errors and this can be done directly in R

#Data summary

NNW_data2 <- na.omit(NNW_data2)
sp_summ <- ddply(NNW_data2, c("species"), summarise, sample_size=length(species), mean_wg=mean(wing), sd_wing=sd(wing), se_wing=sd(wing)/sqrt(length(species)))
sp_summ


#Using the summary data on the figure we created

ggplot(NNW_data2, aes(x=wing, fill=species))+geom_histogram(binwidth=1)+facet_grid(species~., scales="free")+scale_x_continuous("wing chord/mm", breaks=c(30,40,50,60,70,80,90,100,110,120,130,140), labels=c(30,40,50,60,70,80,90,100,110,120,130,140),limits=c(30,140))+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))+theme_bw()+ggtitle("Histograms of Wing Chord by Species")+geom_vline(data=sp_summ, aes(xintercept=mean_wg), linetype="dashed")

#Challenge can you create a for loop that calculates the mean +/- SD  and then plot those lines on your facet graph?


#There is a much simpler way to do this can you figure it out? Think matrix notation.

#====================================================================*
#----Bar Graphs----
#====================================================================*

#Making Bar Graphs

ggplot(sp_summ, aes(x=species, y=mean_wg))+geom_bar(stat="identity")

geom_bar()

#Getting Color Back into the figure

ggplot(sp_summ, aes(x=species, y=mean_wg, fill=species))+geom_bar(stat="identity")+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))

#Removing Legend and adding transparency

ggplot(sp_summ, aes(x=species, y=mean_wg, fill=species))+geom_bar(stat="identity",alpha=0.8)+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))+theme(legend.position="none")

#Adding Errorbars

ggplot(sp_summ, aes(x=species, y=mean_wg, fill=species))+geom_bar(stat="identity")+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))+theme(legend.position="none")+geom_errorbar(aes(ymin=mean_wg-se_wing, ymax=mean_wg+se_wing), width=0.1)

#Why are the error bars so small? Can we manipulate the y-axis to make the scale more appropriate.Excersize can you figure out how to edit the axis ticks, labels, limits and title for the y-axis? Will this help the appearance of the errorbars?

library(scales)
ggplot(sp_summ, aes(x=species, y=mean_wg, fill=species))+geom_bar(stat="identity")+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))+theme(legend.position="none")+geom_errorbar(aes(ymin=mean_wg-se_wing, ymax=mean_wg+se_wing), width=0.1)+scale_y_continuous("Average Wing Chord (mm)",breaks=c(40,50,60,70,80,90,100,110,120), labels=c(40,50,60,70,80,90,100,110,120), limits=c(50,125), oob=rescale_none)

#Alternatively you could plot SD in this case if that makes cleaner graph.

ggplot(sp_summ, aes(x=species, y=mean_wg, fill=species))+geom_bar(stat="identity")+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))+theme(legend.position="none")+geom_errorbar(aes(ymin=mean_wg-sd_wing, ymax=mean_wg+sd_wing), width=0.1)+scale_y_continuous("Average Wing Chord (mm)")

#Now see what happens if you change the fill aesthetic argument to color
ggplot(sp_summ, aes(x=species, y=mean_wg, color=species))+geom_bar(stat="identity")+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))+theme(legend.position="none")+geom_errorbar(aes(ymin=mean_wg-sd_wing, ymax=mean_wg+sd_wing), width=0.1)+scale_y_continuous("Average Wing Chord (mm)")

#Challenge see if you use skills from yesterday to make a new column that indentifies species as resident or migrant and then make a facet grid graph using your new factor.

#====================================================================*
#----More Advanced Data Summaries with Plyr----
#====================================================================*

#Subsetting the data to look at within species patterns

?subset()

NNW_GRCA <- subset(NNW_data2, species == "GRCA")

head(NNW_GRCA)

str(NNW_GRCA)

summary(NNW_GRCA)

NNW_GRCA <- na.omit(NNW_GRCA)

#Challenge now use matrix notation to subset the dataframe


#More advanced data queries and manipulations

GRCA_summ <- ddply(NNW_GRCA, c("sex", "band_age"), summarise, n=length(species), mean_wg=mean(wing), sd_wg=sd(wing), se_wg=sd(wing)/sqrt(length(species)), mean_tars=mean(tars), sd_tars=sd(tars),se_tars=sd(tars)/sqrt(length(species)),mean_mass=mean(mass), sd_mass=sd(mass),se_mass=sd(mass)/sqrt(length(species)))
GRCA_summ

#Lets load another dataframe with some other morphological data on GRCA

GRCA_DC <- read.csv("Data/GRCA_DC.csv", header=TRUE)

head(GRCA_DC)

GRCA_DC2 <- na.omit(GRCA_DC)

head(GRCA_DC2)

#Challenge can you guys write the code to summarize sample size, mean wing and se of wing by age and sex using ddply?

#====================================================================*
#----Scatterplots and Errorbars----
#====================================================================*

#Ok lets make a scatterplot with error bars

ggplot(GRCADC_summ, aes(x=AGE, y=mean_wg, color=SEX))+geom_point()

#Oh no the points are on top of each other and that will not look good once we add errorbars. We need to jitter the points. There are two ways to do this:

ggplot(GRCADC_summ, aes(x=AGE, y=mean_wg, color=SEX))+geom_point(shape=19, size=4,position=position_dodge(width=0.5))+geom_errorbar(aes(ymin=mean_wg-se_wg, ymax=mean_wg+se_wg), position=position_dodge(width=0.5), width=0)

#Now try adjusting the color of the points, modify the ticks and change the theme

#====================================================================*
#----Regressions----
#====================================================================*

#Regressions with ggplot

#Lets start with a basic regression

ggplot(GRCA_DC, aes(x=WT, y=TAR, color=SEX))+geom_point(size=3)+geom_smooth(method=lm, linetype="dashed")+scale_y_continuous(limits=c(25,30))+xlab("Weight (g)")

#What if we want to get rid of the shaded confidence region

ggplot(GRCA_DC, aes(x=WT, y=TAR, color=SEX))+geom_point(size=3)+geom_smooth(method=lm, linetype="dashed", se=FALSE)+scale_y_continuous(limits=c(25,30))+xlab("Weight (g)")

#Other kinds of model fits

ggplot(GRCA_DC, aes(x=WT, y=TAR))+geom_point(size=3)+geom_smooth(linetype="dashed")+scale_y_continuous(limits=c(25,30))+xlab("Weight (g)")

ggplot(GRCA_DC, aes(x=WT, y=TAR))+geom_point(size=3)+geom_smooth(method=glm, linetype="dashed")+scale_y_continuous(limits=c(25,30))+xlab("Weight (g)")

#====================================================================*
#----Text on Figures----
#====================================================================*

#Adding text to a figure

ggplot(GRCA_DC, aes(x=WT, y=TAR, color=SEX))+geom_point(size=3)+geom_smooth(method=lm, linetype="dashed", se=FALSE)+scale_y_continuous(limits=c(25,30))+xlab("Weight (g)")+geom_text( x=48, y=30, size=4, color="black", label="A)")

#Challenge try and change the text in some way (color, size, position, etc.)


#====================================================================*
#----Other Themes using ggthemes----
#====================================================================*

#Other themes and styles

library(ggthemes)

cach_reg+theme_igray()

cach_reg+theme_gdocs()

cach_reg+theme_solarized_2()

cach_reg+theme_wsj()

cach_reg+theme_economist()

cach_reg+theme_few()

#====================================================================*
#----Multiplot Function----
#====================================================================*

#Function for plotting multiple plots on a page

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Creat multiple plots for using with the function

a <- ggplot(GRCA_DC, aes(x=WT, y=TAR))+geom_point(size=3)+geom_smooth(method=glm, linetype="dashed")+scale_y_continuous(limits=c(25,30))+xlab("Weight (g)")

b <- ggplot(GRCADC_summ, aes(x=AGE, y=mean_wg, color=SEX))+geom_point(shape=19, size=4,position=position_dodge(width=0.5))+geom_errorbar(aes(ymin=mean_wg-se_wg, ymax=mean_wg+se_wg), position=position_dodge(width=0.5), width=0)

c <- ggplot(sp_summ, aes(x=species, y=mean_wg, fill=species))+geom_bar(stat="identity")+scale_fill_manual(values=c("#F77F2F","#FAF05F", "#ED361A", "#FCD853", "#F2511F", "#B4DB64", "#2892C7", "#64B0AC" ))+theme(legend.position="none")+geom_errorbar(aes(ymin=mean_wg-sd_wing, ymax=mean_wg+sd_wing), width=0.1)+scale_y_continuous("Average Wing Chord (mm)")

d <- ggplot(GRCA_DC2, aes(x=WG, fill=SEX))+geom_histogram(binwidth=1)+facet_grid(SEX~.)+scale_fill_manual(values=c("#F77F2F","#FAF05F"))+geom_vline(data=GRCADC_summ2, aes(xintercept=mean_wg), linetype="dashed")
d

#Now Use multiplot function to aggregate the four graphs
multiplot(a,b,c,d, cols=2)

#Now see if you can add an a, b, c and d to your figures using geom_text

#====================================================================*
#----Exporting Figures----
#====================================================================*

pdf("Multiplot2.pdf", height=8, width=11)

multiplot(a,b,c,d, cols=2)

dev.off()

#Here is a great additional reosurces for exporting high reolsution figures outside of Rstudio or to control dpi (which is low when using a PC)

#http://blog.revolutionanalytics.com/2009/01/10-tips-for-making-your-r-graphics-look-their-best.html



