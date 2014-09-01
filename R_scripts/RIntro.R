#######################################################################*
# ---- MIGRATORY BIRDS WORKSHOP: INTRODUCTION TO R ----
#######################################################################*
# This document will lead you through the early steps of data exploration and, in 
# the process, you will explore how R interprets a dataset. It is important that 
# you follow every step, formatting and running your data exactly as the script 
# describes. Since R is a script-based program, R syntax is not very flexible; as 
# such, you must pay VERY careful attention to detail when writing scripts.
# One focus of this document (and the workshop in general) is to ensure that you 
# use “best management practices” (BMP’s) in writing scripts. The reasons for some 
# of the suggestions may not be obvious at first but, as you become more familiar 
# with R (and, inevitably, write more complicated scripts) you’re going to begin to 
# understand why this is necessary. 

#  ---- R BMP’s ---- 
# 1. ALWAYS enter your script into the R Editor window rather than the console
#   Why: You will likely use this line of code again or edit the line of 
#   code at some point. Entering your script in R Editor allows you to 
#   both edit and (importantly) retrace your steps easily.

# 2. If you come close to reaching the margins of your editor window, try to make 
#    the code multiple lines (do this even if your OS text editor provides automatic
#    word wrapping)
#   Why: It makes it easier to review and understand your script.

# 3. ALWAYS put a space between lines of code that you will run.
#   Why: This makes both reviewing and running your script much easier. Also, if you 
#   are running a section of code (multiple lines for one function), you do so by 
#   highlighting the whole section with your mouse and clicking CTRL+R (PC) or
#   Command+Enter (Mac). Spaces between lines makes knowing where to begin and 
#   end the section you are running much clearer.

# 4. ALWAYS add comments to your code (using a “#” sign) both between sections 
#   Why: To remember what you did and why (you will forget)!

# 5. Beware of UPPERCASE and lots of punctuation: Whenever possible, use lowercase,
#   easy-to-remember (and type) names and column headings.
#   Why: If you use uppercase for some things just make sure you know how you 
#   named things. R is case sensitive, you need to remember how you entered
#   code.

#======================================================================*
# ---- Data management, the early steps ----
#======================================================================*

# 1. Open up R using the R Studio application. We will now take a few minutes
#    exploring the RStudio interface.

# 2. Ensure that you know the location of your default working directory. 
#   Your working directory is the location to which R reads and writes files 
#   by default. “getwd” is a FUNCTION that provides your current working 
#   directory. All functions have to be followed by parentheses; inside the 
#   parentheses is the argument you want the function to execute. The 
#   function getwd does not require an argument. Type the following into 
#   your R editor window and hit Ctrl+R (PC) or Command+Enter (Mac) to run 
#   the line of script.

getwd()

# Note the color of the text above. This color means that R with run the
# text. You can automatically change the text to a comment by pressing the
# keyboard combindation Ctrl+Shift+C (PC) or Command+Shift+C (Mac). Try to
# enter text and changing it to a comment in the space below.

# 3. List the files in your working directory using the "list.files" function.

list.files()

# R has easy ways to get help. In the space below, type the function "list.files" with 
# and then the tab button on your keyboard. This will bring up a help window that 
# describes the function. You can also use the help tab on the lower left hand 
# window of RStudio.


# 4. You should see a folder named “MigratoryBirds” in your working directory. 
# If it’s not there, you will need to change your working directory. The 
# "setwd" function, below, requires you to provide the address of the 
# MigratoryBirds folder on your computer.

setwd(<insert your working directory here>)

# Note: You can also interactively set your working directory in the files
# tab of the lower right hand window (set under the "More" tab).

# 5. Changing the default directory: You can change the settings for the default
# working directory such that every time you open R it will open in a convenient
# location. To do so, go to RStudio->Preferences->General and input the dirctory
# location for this course.

# 6. Loading the data into R: We will now add the file "encounters.csv" into R.
# from your "Data" folder. We will provide the function "read.csv" with the path to
# the file name in quotation marks. We will also assign a name (encounters) to the
# file using "=".

encounters = read.csv("Data/encounters.csv")

# Notice that there is now an object called encounters in your environment (the
# upper right-hand pane of RStudio).

# Note: There are other ways to assign names other than "=" (my preference), 
# such as:

encounters2 <- encounters

# This creates a new file named encounters2 from encounters. You can also use
# a keyboard shortcut for this, "Alt -". 

# -or-

encounters -> encounters2 # As above, just in the other direction

# Take a look at the history tab of the upper right-hand pane in RStudio, you will see 
# that there is a list of commands that you have entered thusfar.

# 8. A common complaint with early users of R is that you can't "see the data".
# There are several ways in which you can look at the data.

# To view your data as a spreadsheet: In the environment tab of the upper 
# right-hand window, click on the encounters dataset.

# To view the first few lines of your dataset in the Console, run the function:

head(encounters)

# To view just the column names of your dataset, run the function:

names(encounters)

# To view what format your dataset is (more on this later), run the function:

class(encounters)

# To view the types (eg., numeric, integer, factor, character) of variables 
# in your dataset, run the function:

str(encounters)

#======================================================================*
# ---- Dataframes and Matrices ----
#======================================================================*

# The bread-and-butter data formats in R are matrices and data frames. A
# matrix is a set of values arranged in rows and columns. A data frame
# is a matrix that contains column names. To illustrate explore the utility
# of this, we will create a matrix, then convert it to a dataframe. 

# 1. First, we will create vectors of values using the "c" function (note:
# "c" stands for concatenate). The ":" below means that we want a string
# of integers from 1:6.

v1 = c(1:6)

# Type v1 in the space below to see the values you created.

# Tired of re-typing the name of the data to see the values? You could
# wrap the above in parentheses to run the function and view its 
# output, as below.

(v1 = c(1:6))

# 2. Let's create a two vector of factor variables using the rep function 
# and then paste them together using "paste".

v2A = rep('A',3)

v2B = rep('B',3)

v2 = c(v2A, v2B)

# Note: alternatively, we would have done this in one step using:

v2 = rep(c('A','B'), each = 3)

# See the help file for the above function by typing and running the
# code ?rep in the space below.

# 3. Now let's turn these two vectors into a two-column matrix:

m = matrix(c(v1,v2), ncol = 2)

# Type m (or wrap the function above in parentheses) to view your
# output. Note that you have created a 6 row x 2 column matrix 
# of data. Also note how the columns and rows are notated. The
# format is [row, column] and understanding this format is very
# important when querying and subsetting data.

# 4. Explore the values of the matrix:

# To view the 2nd row of data for both columns:

m[2,]

# To view the 2nd column of data across rows:

m[,2]

# To view the cell value in row 3 of column 2:

m[3,2]

# 5. We can convert the data frame (using the "data.frame" function). 
# Having column names is a big (necessary) advantage of data frames 
# and we can still use cell assignments. Create a new data frame:

df1 = data.frame(v1,v2)

# Change the column names as below:

names(df1) = c('integer','factor')

# Perhaps you want to change just the 1st column name? You can
# tell R to change just the first column as such:

names(df1)[1] = 'int'

# 6. Extracting data from data frames. To extract a column of data in a 
# data frame, you can use "$" to call the column:

df1$int

# Alternatively, you can still use matrix notation, either using
# the cell assignments as you would with a matrix:

df1[,1]

# Or using the column name:

df1[,'int']

# 7. It is sometimes necessary to convert between data frames and matrices
# to run a certain function.

# Data frame to matrix:

as.matrix(df1)

# Matrix to data frame:

as.data.frame(m)

# Note: because we haven't assigned new object names, we haven't changed the
# structure of either df1 or m. If you would like to change the structure,
# you must assign a new name. Try assigning object names to the above and
# running the function "class" to see how your data are classified. Also
# note the naming structure from the as.data.frame function. The default
# assigned column names aren't useful, therefore should be changed.

# 8. Adding a column to a data frame can be done using matrix or
# data frame notation. Let's start by making another vector of values
# and adding it to the data frame df1.

v3 = c('ROPI','HOSP','NOCA','AMRE','COGR','HOWR')

# Matrix notation:

df1[,3] = v3

# Notice in the above the column name that was assigned by default.

# Data frame notation:

df1$V3 = v3

# Again, notice the assignment of the column name.

# Note: rather than doing this in two steps, you can do it in one,
# such as the below example using data frame notation.

df1$V3 = c('ROPI','HOSP','NOCA','AMRE','COGR','HOWR')

# Challenge: Change the name of just the new column to "alpha.code".

# You can also remove a column from a data frame. To remove the third column
# of the data frame:

df2 = df1[,-3]

df2

#======================================================================*
# ---- Using Matrix notation to query and subsetting data ----
#======================================================================*

# The real advantage of understanding the dual nature of a data frame 
# matrix notation is in querying and subsetting data. Here we use common
# boolean operators to extract values from the data frame we created.

# 1. Extract all row values values where the factor row of data frame df1
# contains an "A"

df1[df1$factor == 'A',]

# The "==" means "is equal to". By leaving the column assignment blank
# after the comma, we are telling R we want all of the columns.
# Alternatively, we can return just the first column:

df1[df1$factor == 'A',1]

# We can also query values for A by asking R to return cell values that
# are not equal to "B" using "!=", where the exclamation point means not.

df1[df1$factor!= 'B',]

# 2. We can also query the dataframe based on the integer. Try to run
# these lines of code, then explore other boolean statements you might 
# make.

df1[df1$int > 2,]

df1[df1$int >= 2,]

# Let's look at the average of values in the df1 and then subset to 
# values greater than the mean:

mean(df1$int)

df1[df1$int>mean(df1$int),]

# Challenge: extact just the species (alpha.code) where the integer
# values are greater than the mean. Recall that there are two ways
# to do it, try both.

# 3. Finally, we can construct more complex queries using "and"("&") and
# "or" ("|") statements. 

df1[df1$int > 1 & df1$factor == 'A',]

df1[df1$int < 3 | df1$int >4,]

#======================================================================*
# ---- Summarizing data ----
#======================================================================*

# We will now return to our encounters dataset to explore different ways
# that data frame may be summarized. In a later lesson, you will use
# the R-package "plyr" for more complex dataset summarizing.

# 1. The summary function is used to provide a basic summary of each of
# the variables in a data frame.

summary(encounters)

# We can also just summarize one variable, let's summarize the mass column 
# in the encounters data frame.

summary(encounters$mass)

# 2. We may be interested in the number of records, or the number of records
# that satisfy a condition.

# To determine the dimensions of a data set, we use the function "dim":

dim(encounters)

# If we just want the number of rows, we would use:

dim(encounters)[1]

# -or, (sometimes) more simply, just choose a column-

length(encounters$band_num)

# Challenge, you could also determine the number of rows using matrix notation
# try to do so below:


# We can combine the query operation from the above section to determine the
# number of female birds that have been observed:

length(encounters[encounters$sex == 'F',1])

# Note: It doesn't matter what column you tell it. Try entering any of the
# columns numbers or names. It will give the same answer. Why?

# What if we want to know the number of species in the dataset? We can use
# the "unique" function, in combination with length:

# Try to use the unique function by itself first:

unique(encounters$species)

# Then determine the number of species in the data frame:

length(unique(encounters$species))

# To count the number of records across a given factor level, we can use
# the table function:

table(encounters$sex)

# 4. We will likely be interested in numerical summaries of data by a given
# factor. For example, how do wing chord lengths vary between males and 
# females? There are two functions that can be used for this, tapply and 
# aggregate.

# Let's use the "tapply" function to determine the average wing chord 
# lengths for male and female birds:

tapply(encounters$wing, encounters$sex,mean)

# Notice how we have a lot of NA's in this summary data. NA's for a
# given factor level made wing impossible to calculate the means for
# that level. We can ignore the NA's in this function, using the 
# argument "na.rm = T". As below:

tapply(encounters$wing, encounters$sex,mean, na.rm = T)

# Not very useful without species data. We can use our boolean subsetting
# technique to look at the average wing chord for a given species.
# Let's use the Gray catbird for our example:

tapply(encounters[encounters$species == 'GRCA','wing'],
       encounters[encounters$species == 'GRCA','sex'],mean, na.rm = T)

# Well that's a lot of script to write! Let's use aggregate to see average
# wing chords by species and sex:

aggregate(encounters$wing, by = list(encounters$species, encounters$sex), 
          mean, na.rm = T)

# Try to change the order of species and sex and view the difference
# in the results frame.

#======================================================================*
# ---- Recoding variables ----
#======================================================================*
# You likely noticed that there's a problem with our data summaries so
# far! Some of the sexes are listed as lower or upper case! R treats 
# these as different factor levels. There are a few different ways that 
# we can recode variables.

# 1. For categorical (nominal) variables simply assign a new value using 
# matrix notation. We will change male bird sex data from the lowercase
# "m" to uppercase "M".

# Tabulate encounters by sex:

table(encounters$sex)

# Use matrix notation to assign the uppercase "M" to "m" values:

encounters[encounters$sex == 'm','sex'] = 'M'

# Look at your new table:

table(encounters$sex)

# Did you notice that the "m" is still there, but now has no records?
# you can remove unused factor levels as such:

encounters$sex = factor(encounters$sex)

# Verify that the change has been made:

table(encounters$sex)

# 2. There a base function that automatically changes lowercase to uppercase
# you can use the function as below:

encounters$sex = toupper(encounters$sex)

encounters$sex = factor(encounters$sex)

table(encounters$sex)

# 3. If you would like to group a numeric variable, the ifelse function is
# useful. For example perhaps you want to create two weight classes for the
# American robin, "heavy" and "light" to represent birds that are above and
# below average body mass.

# First, in the space below, use the data-subsetting steps above to create
# a data frame that includes only American robin (AMRO). Assign the name 
# "amro" to your new data frame (i.e., amro = <data subset code here>).

# Now, we'll explore two ways the ifelse statement can be constructed. 
# Let's first calculate the mean:

mean(amro$mass, na.rm = T)

# Note: Once again, "na.rm = T" removes NA records, which is necessary 
# tocalculate means.

# The mean value is 74.69245. We will now write our ifelse statement to
# classify any birds heavier than this value as "heavy". Note:  
# recall that typing "ifelse" then hit tab on your keyboard provides
# a brief help file on the command.

amro$weight.class = ifelse(amro$mass > 74.69245, 'heavy','light')

# We could also combine the above statement into one, as:

amro$weight.class2 = ifelse(amro$mass > mean(amro$mass, na.rm = T),
                            'big','small')

# We can also use the ifelse function to recode a factor variable,
# as below:

encounters$sex = ifelse(encounters$sex == 'm','Male',encounters$sex)

# Ifelse statements can also be nested (note that I like to put these
# on multiple lines, which I feel helps display what is being done):

encounters$sex = ifelse(encounters$sex == 'f','Female',
                        ifelse(encounters$sex == 'u','Unknown',
                               encounters$sex))

# Challenge: Using one of the three methods above, for each species determine
# the average body mass by just HY and AHY classes (hint: data 
# subsetting and recoding is necessary and use the aggregate function 
# because there are multiple factor levels).

#======================================================================*
# ---- Writing your own functions  ----
#======================================================================*

# Canned functions, and the numerous functions available in various
# R packages really make R an easy environment to work in. When coding,
# however, it often saves a lot of time and work to write your own 
# functions or combine pre-existing functions into a larger function.

# 1. To illustrate function structure, we'll create a silly function that 
# multiplies any two numbers and outputs the result.

multiplier = function(x,y){
  x*y
}

# We've assigned the name "multiplier" to our function with the "=" sign. 
# "function(x,y)" tells R that you want the inputs to be x and y
# The curly brackets are necessary when a function is > 1 row
# The item between the curly brackets is the function that is 
# executed.

# Run the function as you would any other, let's multiply 2 x 3

multiplier(2,3)

# 2. As a less silly example, that I actually use quite a bit, let's
# write a standard error function that combines two provided functions,
# standard deviation and length.

# Let's extract the wing chord length for the Carolina chickadee for 
# this example:

cach.wing = encounters[encounters$species == 'CACH','wing']

# We can determine the standard deviation of wing chord length as:

sd(cach.wing)

# It didn't work! Why? Have to remove the NA's. na.omit is a function
# that removes NA values from the vector.

cach.wing = na.omit(cach.wing)

sd(cach.wing)

# We can determine the number of records as:

length(cach.wing)

# We can calculate the standard error as:

sd(cach.wing)/sqrt(length(cach.wing))

# Or make a more generalizable function to calculate the standard 
# error across any vector of values:

se = function(x){
  x = na.omit(x)
  sd(x)/sqrt(length(x))
}

# Note assignment

# Challenge 1. Write a function that subsets the encounters data frame by
# species and removes any NA's from the data frame.

species.subset = function(species){
  <your function here>
}

# To execute your function:

cach = species.subset('CACH')

#############################################################################*
# ---- ADDITIONAL FUNCTION TO EXPLORE !---- 
#############################################################################*
# Were you able to get through that challenge without a hitch? If so, try 
# the addtional challenge below!

# Challenge 2 (difficulty = high!). Write a function that can calculate and output 
# the mean and standard error for the wing length of any species (one at a time) 
# within the encounters data frame. Hint, include subsetting in your function. 
# Here's a start:

mean.se = function(species){
  <your function here>
}

# To execute your function

out = mean.se('CACH')

#======================================================================*
# ---- For loops  ----
#======================================================================*

# The for loop. The for loop is simply the biggest time saver, and time
# sink, of the coding world. It should be noted that R, as a language,
# is not very efficient with for loops (they can take a lot of memory and
# and can even crash R -- especially if you've written an "infinite" loop!).
# Because of this, there are often more effective alternatives to writing
# them. That being said, knowing how to create and run for loops is crucial
# for code writing!

# 1. A silly example: We will create a for loop that simply multiplies the
# values in a vector by 2. 

# Let's start by writing a vector of integers from 2 to 20 by 3:

v = seq(from = 2, to = 20, by = 2)

# Let's look at the value of v in position 1 (indexing):

v[1]

# Try to look at another position within the frame.

#  Now create an empty vector to store your new values:

v2 = numeric()

# Now we write the for loop:

for (i in 1:length(v)){     
  v2[i] = v[i]*3            
}

# And look at the output:

v2

# 2. Fibonacci wrote a formula that many regard as the first population model
# (he was modeling rabbit population growth). This formula (known better as 
# the Fibonacci number series), turned out to be fundamental to many
# environmental processes. Here, we will create a for loop that executes Fibonacci's
# formula, generating 10 numbers (stopping rules are necessary!). To do
# so, we'll first create a vector of starting values, then fill the vector 
# as below.

fibonacci.sequence = c(1,1)

for(i in 3:10){
  fibonacci.sequence[i] = fibonacci.sequence[i-1]+fibonacci.sequence[i-2]
}

fibonacci.sequence

#############################################################################*
# ---- ADDITIONAL FOR LOOP OPERATIONS TO EXPLORE !---- 
#############################################################################*

# 3. We can also create a for loop that executes a function. For example,
# let's use the mean function and standard error function we created to 
# calculate the standard error in wing measurements for each species (this
# example is still silly, because we've already learned easier ways to 
# do this).

# We'll start by collecting the unique species assignments in the data frame:

sp.list = unique(encounters$species)

# Next we'll write a function that subsets the data frame:

df.species.sub = function(i){
  na.omit(encounters[encounters$species == sp.list[i],'wing'])
}

# Create an empty data frame to fill. This data frame will
# have three columns: species, mean, and standard error. The
# for loop will fill rows with these values for each species.

df = data.frame()

# And execute a for loop that fills the data frame:

for (i in 1:length(sp.list)){
  df[i,1] = sp.list[i]
  df[i,2] = mean(df.species.sub(i))
  df[i,3] = se(df.species.sub(i))
}

df

# 4. We can also put a for loop inside of a function. Let's make a loop
# that calculates the above for a given age class:

age.wings = function(age.class){
  # Subset the frame by age class:
  encounters =  encounters[encounters$band_age == age.class,]
  # Generate the species list:
  sp.list = unique(encounters$species)
  # Empty data frame to fill:
  df = data.frame()
  # For loop:
  for (i in 1:length(sp.list)){
    df[i,1] = sp.list[i]
    df[i,2] = mean(df.species.sub(i))
    df[i,3] = se(df.species.sub(i))
  }
  # Return the data frame
  df
}

age.wings('AHY')
age.wings('HY')

# A final challenge! Take a look at the values associated with mean wing
# lengths for the species. Is there something wrong? In the space below
# write some code that will calculate the wing lengths by species and age then, 
# if necessary, fix the for loop to calculate the correct values.
