#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

# Get files:

prop.df = read.csv('output/prop.df.csv')
nTime = read.csv('output/Ntime.df.csv')
outSummary = read.csv('data_source_table.csv')

# Source core-transient functions:

source('scripts/R-scripts/core-transient_functions.R')

# Load libraries:

library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(MASS)

#----------------------------------------------------------------------------------*
# ---- Tokeshi functions ----
#==================================================================================*

# Tokeshi's function to determine if there is a greater number of individuals in a
# bin than random chance (F>f):

tokeshi.rl.fun = function(N, right.or.left, h){
  outs = NULL
  ins = right.or.left:N
  for(i in ins){
    o = (factorial(N)/(factorial(i)*factorial(N-i)))*h^i*(1-h)^(N-i)
    outs = c(outs, o)
  }
  return(sum(outs))
}

# Tokeshi's function to determine if there are left or right modes that are
# greater than expected under a null distribution:

tokeshi.c.fun = function(N, nr, nl, h){
  outs = NULL
  for (i in nl:(N - nr)){
    for(j in nr:(N - i)){
      o = (factorial(N)*h^(i + j)*(1-2*h)^(N-i-j))/
        (factorial(i)*factorial(j)*factorial(N-i-j))
      outs = c(outs, o)
    }
  } 
  sum(outs)
}

# Function to run Tokeshi's bimodality test for a given site:

tokeshiFun = function(site, h){
  d = read.csv('output/prop.df.csv')
  df = d[d$site == site, ]  # Subset to a given site
  N = length(df[,1])              # The total number of species at the site
  h = h                           # The frequency interval 
  nr = length(df[df[,4]>=1-h,1])  # The number of species in the upper class
  nl = length(df[df[,4]<=h,1])    # The number of species in the lower class
  Pc = tokeshi.c.fun(N, nr, nl, h)    # Probability of left-or-right skew
  Pr = tokeshi.rl.fun(N, nr, h)       # Right mode probability
  Pl = tokeshi.rl.fun(N, nl, h)       # Left mode probability
  out.df = data.frame(site, N, h, nr, nl, Pc, Pr, Pl)
  return(out.df)
}

# Wrapper function to run Tokeshi's bimodality test across sites. This is used
# in the construction of the Tokeshi plot:

tokeshiWrapper = function(h){
  d = read.csv('output/prop.df.csv')
  site = unique(d$site)
  list.out = list()
  for(i in site){
    list.out[[i]] = tokeshiFun(i, h)
  }
  t1 = na.omit(rbind.fill(list.out))
  t1$bimodality = ifelse(t1$Pl<=0.05 & t1$Pr<=0.05, 'strongly bimodal',
                         ifelse(t1$Pl<0.25&t1$Pr<0.25, 'bimodal',
                                ifelse(t1$Pl<0.5&t1$Pr<0.5, 'weakly bimodal',
                                       ifelse(t1$Pl<=0.05&t1$Pr<=0.5, 'weakly bimodal',
                                              ifelse(t1$Pl<=0.5&t1$Pr<=0.05, 'weakly bimodal', 'not bimodal')))))
  return(t1)
}

#----------------------------------------------------------------------------------*
# ---- Function to make Tokeshi plot  ----
#==================================================================================*

tokeshiPlot = function(h){
  tokeshi.outs = tokeshiWrapper(h)
  ggplot(tokeshi.outs, aes(x = Pr, y = Pl,col = bimodality)) +
    geom_point() +
    xlab('P(F > f), core species')+
    ylab('P(F > f), transient species')+
    geom_segment(aes(x = .05, y = 0, xend = .05, yend = 1), 
                 color = 1, size = .5, linetype  = 1) +
    geom_segment(aes(x = 0, y = .05, xend = 1, yend = .05), 
                 color = 1, size = .5, linetype = 1) +
    geom_segment(aes(x = .25, y = 0, xend = .25, yend = 1), 
                 color = 1, size = .5, linetype  = 2) +
    geom_segment(aes(x = 0, y = .25, xend = 1, yend = .25), 
                 color = 1, size = .5, linetype =2) +
    geom_segment(aes(x = .5, y = 0, xend = .5, yend = 1), 
                 color = 1, size = .5, linetype  = 3) +
    geom_segment(aes(x = 0, y = .5, xend = 1, yend = .5), 
                 color = 1, size = .5, linetype = 3) +
    ggtitle('Tokeshi bimodality test across sites')+
    # Add themes:
    theme(axis.text = element_text(size=14, color = 1),
          axis.title.x = element_text(vjust = -1),
          axis.title.y = element_text(vjust = 2),
          title = element_text(size=18, vjust = 2, face = 'bold'),
          axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          plot.margin = unit(c(2,2,2,2), "lines"))
}

# ---- Tokeshi output table ----
# Input is the cut-ff for core- and transient designation

tokeshi = tokeshiWrapper(1/3)

# ---- Write Tokeshi output table to file ----

write.csv(tokeshi, 'output/tabular_data/tokeshi.table.csv', row.names = F)

#----------------------------------------------------------------------------------*
# ---- Tokeshi plot  ----
#----------------------------------------------------------------------------------*

# Plot output:
# NOTE! THERE ARE WARNINGS HERE ... EXPLORE WHY!!!

tokeshiPlot(1/3)

# Write plot to file:

pdf('output/plots/tokeshi.pdf', width = 8, height = 6)
tokeshiPlot(1/3)
dev.off() 

