# USEFUL OPTIONS and RULES #################
#change default number of decimal places; 7 places if the default
options(digits = 2)

# vector indices start at 1 (NOT ZERO)
# NA is missing, "NA" is a string

#Getting details on every possible object
attributes(x)
str(x)

##################### DOWNLOADING PACKAGES ##########################

install.packages("dplyr")
install.packages("ggplot2")
install.packages("imputeMissings")
install.packages("openxlsx")
install.packages("pastecs")

library(dplyr)

# SYSTEM STUFF ###################

R.version
getwd()


# HELP ###########################
?read.csv #input appears in the help tab, bottom right



# THE SIX BASIC DATA TYPES/CLASSES #####################
  # Numeric
  # Integer
  # Complex
  # Character
  # Factor
  # Logical

 
#Numeric #####################################
num_data <- c(3, 7, 2)
class(num_data)

num_data_dec <- c(3.4, 7.1, 2.9)
class(num_data_dec)


#Integer #####################################

children <- c(3, 5, 7, 2, 0, -1)
class(children)

children <- as.integer(c(3, 5, 7, 2, 0, -1))
class(children)


#character  ##################################
chars <- "test value"
class(chars)

otherchars <- as.character(children)

#entire vectors become character vectors 
intvector <- c(1,3,5,7,9,-3)
class(intvector) #numeric at first
intvector <-   c(intvector, 99, 101, 301, "charlier")

intvector

####################FACTORS ########################################
#THESE ARE QUALITIES more than just data values like street address
#A vector with a list of levels/categories.
#Qualitative: eye color, gender, marital status

#use factors for character data with a limited number of discrete values
gender <- factor(c("male", "female", "female", "female", "male", "male"))


#levels returns the distinct values in a vector of factors
levels(gender)

#gets the number of levels
nlevels(gender)

#reference level is  always the first level.  To change that: RELEVEL
levels(gender)
gender <- relevel(gender, ref="male")
levels(gender)

class(gender)

#Example 1 of: convert a vector of character strings to a factor vector

jobTitles <- c("manager", "front-line worker", "manager", "manager", "driver", "driver", "front-line worker")
jobTitleAsFactors <- as.factor(jobTitles)

levels(jobTitleAsFactors)

#Example 2 of: convert a vector of discrete values into a factor
v <- c(1, 1, 0, 1, 0)

v2 <- factor(
        v,
        levels = c(0, 1),
        labels = c("bad", "good")  #ordering of levels and labels is important
)

v2

#Ways to get frequencies of each level in a vector
table(v2)
summary(v2)

#ways to get proportions for each level in a vector
prop.table(table(v2))
prop.table(summary(v2))


#convert factors (which are actually numeric under the hood) 
#into their numerical equivalent :
v3 <- as.numeric(v2)

# and can convert numeric vectors into factors different ways
as.factor(v3)
factor(v3)

#FACTOR function lets you supply labels at the same time
num <- 1:5
num
numAsFactor <- factor(
                num,
                labels = c("bad", "poor", "neutral", "good", "very good")
               )

numAsFactor

# LOGICAL ##########################################
# REMEMBER: TRUE and FALSE must be uppercase to be logical
# FALSE is equivalent to ZERO
# TRUE is all other values

logicals <- c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, 3<5)
logicalsAsInts  <- as.numeric(logicals)

logicalsAsInts

intsAsLogical <- as.logical(logicalsAsInts)
intsAsLogical

logInts <- c(-1, 3, 0, 5, 0, 44)
as.logical(logInts)

# DATES and TIMES ################################################
#default date format is ISO8601: 2001-02-13 (yyyy-mm-dd)
as.Date("01/10/16", "%d/%m/%y")

dates <- c("02/27/92", "02/27/99", "01/14/92")
times <- c("23:03:20", "22:29:56", "01:03:30")
x <- paste(dates, times)
y <- strptime(x, format= "%m/%d/%y %H:%M:%S")
y

#extracting weekdays, months, quarter, and years from date
weekdays(y, abbreviate = FALSE)
months(y, abbreviate = FALSE)
quarters(y, abbreviate = FALSE)
format(y, "%Y") #4-digit years
format(y, "%y") #2-digit years

# WORKING DIRECTORY #####################################

setwd("D:/R Projects/RBasics")
getwd()



# IMPORTING DATA - CSV TEXT FILE ########################
# REMEMBER: file names are case-sensitive
dat <- read.csv(
  file = "data.csv",
  header = TRUE,
  sep = ",",
  dec = ".",  #indicates the decimal point in your language
  stringsAsFactors = TRUE #FALSE by default starting with R 4.0.0
)

View(dat)
head(dat) #first six rows


###  https://statsandr.com/blog/data-manipulation-in-r/

# VECTORS #####################################

# concatenate a sequence:
seq1 <- c(1:10)

seq1 <- c(seq1, 1, 5 / 6, 2^3, -0.05)

seq1

#create vectors from more complex sequences than just 1:10
seq(from = 2, to = 5)
seq(from = 2, to = 5, by = 0.5)
seq(from = 2, to = 5, length.out = 14) #this means you will get 14 values; the BY is computed for you.

#create vectors as repetitions of specific values
rep(1, times=3)

#The first vector are the values, the second vector are how many times to repeat each
rep(c("A", "B", "C"), times = c(3, 1, 2))

#mixing strings and numbers; if any strings are in a vector, the vector is ALL string
x <- rep(c("A", 2, "C", 8, "HARLEY", 155, "zero" ), times = c(3, 1, 2, 7, 6, 2, 2))

#selecting specific values in a vector
x[3]
#selecting multiple vector values
x[c(1,3,5, 8, 2)]

x

x[c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)]

# removing elements; here, removing elements 2 and 12-17
x[-c(4,seq(from=14, to=19))]

#type of the vector:
class(x)
is.numeric(x) #returns TRUE or FALSE
is.logical(x)
is.character(x)
is(x)

#length of vector
length(x)

#select the very last element of a vector
x[length(x)]

################changing a vector's type or length
as.character(seq1)
as.logical(seq1)

#!!!!!!!!!!!!! you can actually 
#truncate the vector by shortening the length
seq1 <- c(1:10)
seq1 <- c(seq1, 1, 5 / 6, 2^3, -0.05)

length(seq1) <- 3
seq1

#vector numerical operations - combining vectors mathematically
#MUST BE same length

seq1 <- c(1:10)
seq2 <- c(21:30)

seq1+seq2
seq1*seq2
seq1-seq2
seq1^seq2

#vector functions
max(seq1)
min(seq2)
sum(seq1)
prod(seq1)
cumsum(seq1)
cumprod(seq1)
seq1*-1
abs(seq1*-1)
#also sqrt, cos, sin, tan, log, log10, exp, abs

# can ROUND, FLOOR, CEILING all elements at once
exp(seq1)
round(exp(seq1))
floor(exp(seq1))
ceiling(exp(seq1))


# LOGICAL OPERATOR ##############################
# relational: <, >, >=, <=, ==, !=
# OR: |
# And: &
# Negation: !

# operations using logical operators applied to entire 
#vector in one expression
!(as.logical(seq1))
(seq1==1|seq1==10)

# ALL and ANY applied to vectors
# do any/all elements of the vector meet the condition?

seq1 <= 1  #applies test to every element

all(seq1 <= 1)
any(seq1 <= 1)


#########character vector operations

#PASTE with separator
code <- paste(c("BE", "BE", "FR", "EN", "BE"), 1:5, sep = "/")
code

code2 <- paste(c("BE", "BE", "FR", "EN", "BE"), 1:5,  sep = "")
code2

#position of elements containing a given string
grep("FR", code2)

#substr applied to entire vector; here, first 3 characters every element
substr(code2, start=1, stop=3)

#substitute/replace characters in a vector of strings
#every element
sub(pattern="BE", replacement="XX", code2)

#SPLIT
cities = c("gadsden, AL", "birmingham, AL", "atlanta, GA", "richmond, VA")
cities
splitup <- strsplit(cities, split=", ")

splitup

#UPPER and LOWER
toupper(cities)
tolower(cities)


# SORTING/RANKING VECTORS ################

sort(cities)
sort(seq1)
sort(seq1, decreasing=TRUE)

#ORDER gives you the element indices for the sorted vector
#from smallest/lowest to largest
order(cities)

order(cities, decreasing=TRUE)

testseq <- c(1.5, 1.5, 3, 2, 5, 17, -3, 1.5, 2)
testseq
order(testseq)

#Each items's rank from smallest to largest, so the one
#ranked as 1 is the smallest and ties all have same rank.
rank(testseq)

################# LISTS ############################
# A list is a VECTOR of objects of any and all types

#Create a list of 3 objects, all of which are vectors
tahiti <- list(
  plane = c("Airbus", "Boeing"),
  departure = c("Brussels", "Milan", "Paris"),
  duration = c(15, 11, 14)
)

tahiti

#Element extraction:

tahiti$departure
tahiti$de   #wtf? this works
tahiti[[2]]
tahiti[["departure"]]

#drilling into specific elements:
tahiti[2][1] #really not the right way 
tahiti[[2]][1] #right way


#transform a list into a vector:
vec <- unlist(tahiti)
vec

randovec <- list(
  plane = c("Airbus", "Boeing"),
  departure = c("Brussels", "Milan", "Paris"),
  duration = c(15, 11, 14),
  aNumber = 3,
  aPrez = "abraham lincoln"
)
randovec

randovec$plane[2]
randovec[[1]][2]
randovec[3]
randovec[[3]]
as.character(randovec[4])

attributes(randovec)
str(randovec)

attributes(randovec$aPrez)
attributes(randovec$departure)
str(randovec$aPrez)

#################ATTRIBUTES###########################
# https://statisticaloddsandends.wordpress.com/2020/10/19/attributes-in-r/



################# DATA FRAMES #########################
#Columns can be from different classes
#Columns = variables
#Rows = observations
#Cells = one value

dat <- cars
dat

#get matrix size
ncol(dat)
nrow(dat)
dim(dat)

#names of lines and columns
dimnames(dat)

#column names only
names(dat) 
colnames(dat) 

#rownames only
rownames(dat)


# SUBSETTING DATAFRAMES
head(dat, n=12)
tail(dat, n=15)

library(dplyr)
#random sample of rows
sample_n(dat, 4, replace=FALSE)

###################### subsetting by rows,columns ####################

dat[3, ] #row 3
dat[, 2] #col 2
dat[3, 2] #single cell row 3 col 2
dat[c(1:5, 10, 18), ] #only rows 1-5, 10, and 18
dat[-c(5:32), ] #all rows EXCEPT 5-32
dat[nrow(dat), ] # only the very last row

#by column name - 3 equivalent ways
dat$speed
select(dat, speed) #dplyr
select(dat, -dist) #dplyr

#query-like subsetting
subset(dat, dat$speed > 20)  #base R subset(dataframe, criteria)
subset(dat, dat$dist <= 50 & dat$speed == 10)  #AND
subset(dat, dat$dist <= 50 | dat$speed == 10)  #OR
subset(dat, dat$dist != 50 & dat$speed != 10)  #not equal

#split by factor into one list per factor level
split(dat, dat$factor_variable)  #need to add example

#Creating a new variable: adding more values to a dataframe

#numeric
dat$speed_dist <- dat$speed * dat$dist
dat
#factor
dat$speed_cat <- factor(ifelse(dat$speed > 7, "high speed", "low speed"))
dat

#####transform continuous into categorical
#breaking speed into open-on-right ranges 0-12, 12-15, 15-19, 19-26+

dat$speed_qualitative <- 
    cut(dat$speed,
        breaks = c(0,12,15,19,26), #cutoffs for each range
        right=FALSE       #open on the right
        )
dat

#ROW means/sums
#completely non-sensical example here:
dat$mean_score  <- rowMeans(dat[, 1:3]) #use cols 1-3 in calculation
dat$total_score <- rowSums(dat[, 1:3])  #use cols 1-3 in calculation

dat
colnames(dat)

#COLUMN means/sums - two ways, colMeans/colsSums handles many cols at once
colMeans(dat[, 1:3])
mean(dat$speed)

colSums(dat[, 1:3])
sum(dat$speed)


#CATEGORICAL VARIABLES and label management

#create a numeric category based on values/range:
dat$dist_cat <- ifelse(dat$dist < 15, 1, 2)
dat

# change from numeric category to FACTOR and specify the labels
# also replaces the numeric dist_cat column just created
dat$dist_cat <- factor(dat$dist_cat,
                       levels = c(1, 2),
                       labels = c("small distance", "big distance") # follow the order of the levels
)


head(dat)
class(dat$dist_cat)

#Alternative way to factorize many variables in one command:
dat <- within(dat, {
  speed_cat <- factor(speed_cat, labels = c(
    "high speed",
    "low speed"
  ))
  dist_cat <- factor(dist_cat, labels = c(
    "small distance",
    "big distance"
  ))
})

#another way to factorize multiple at once:
library(ggplot2)

mpg <- transform(mpg,
                 cyl = factor(cyl),
                 drv = factor(drv),
                 fl = factor(fl),
                 class = factor(class)
)

class(mpg$cyl)


########## relabeling categoricals
head(dat$dist_cat)
dat$dist_cat <- recode(dat$dist_cat,
                       "small distance" = "short distance",
                       "big distance" = "large distance"
)
head(dat$dist_cat)

#################### CHANGING REFERENCE LEVELS OF CATEGORICALS
# R default is levels are ordered alphabetically 
#    or by numeric value when a numeric is transformed into a factor

#get current ordering of levels:
levels(dat$dist_cat)  #first one listed is the reference level

#change the reference level:
dat$dist_cat <- relevel(dat$dist_cat, ref="large distance")
levels(dat$dist_cat)


############# renaming columns/variables in a dataframe #######

dat <- rename(dat,           #dplyr
              distance = dist,
              speed_distance = speed_dist,
              distance_cat = dist_cat
)

names(dat)
head(dat)


############### COMPOSING A DATAFRAME MANUALLY ################
# if any of these concats have less than 4 values, R cycles through
# the values for each row BUT you each must be a factor of 4, so
# the catnames concat has to have 1, 2, or 4 values;  Cannot have 3
# numeric columns will NOT repeat and different # of values results in
# error

dat <- data.frame(
  "variable1" = c(6, 12, NA, 3), # presence of 1 missing value (NA)
  "variable2" = c(3, 7, 9, 1),
  "catnames" = c("molly", "jolly", "holly", "dolly")
)


################### MERGING DATAFRAMES ###############################

#typically data frames are merged using common columns
#but you can specify the merge column names

dat1 <- data.frame(
    person = c(1:4),
    treatment = c("T1", "T2")
)

dat2 <- data.frame(
  patient = c(1:4),
  age = c(56, 23,  19, 44),
  gender = c("M", "F", "F", "M")
)

datx <- data.frame(
  person  = c(1:4),
  dogname = c("poochie", "fido", "charles", "rex")
)

#merge on the person/patient columns

mdat <- merge (
  x=dat1, y=dat2,
  by.x = "person", by.y = "patient",
  all=TRUE
)

mdat

#adding rows/observation from a third data frame using RBIND

dat3 <- data.frame(
  person = 5:8,
  treatment = c("T3")
)

dat3

#binds rows from dat3 into dat1, at the end of dat1
rbind(dat1, dat3)

#RBIND WILL FAIL if the column names are not the same
#RBIND WILL FAIL if the number of columns is not the same
rbind(dat1, datx)
rbind(dat1, dat2)


#adding columns/variables from one dataframe to another

cbind(dat2, dat3) #adds all the columns alongside, no join condition
datmess <- cbind(dat1, datx$dogname) #adds a single column alongside

datmess


library(dplyr)
#dplyr - have to use quotes if the old column name looks like a selection
#PATTERN IS NEW NAME = OLD NAME
datmess <- rename(datmess,  
              #NEW NAME = OLD NAME
              pet_name = "datx$dogname" ,
              pet_parent = person,
              "Which Med?" = treatment)

datmess


#another way to add a column from another frame AND rename in one step

data.frame(datmess,
           treatment = dat2$gender
)


########################### MISSING VALUES ###################################

#represented as NA
#often when an NA enters into a calculation, an NA is also the result of the calculation

dat <- data.frame(
  "variable1" = c(6, 12, NA, 3), # presence of 1 missing value (NA)
  "variable2" = c(3, 7, 9, 1),
  "catnames" = c("molly", "jolly", "holly", "dolly")
)

mean(dat$variable2)
mean(dat$variable1)  #result is NA due to NA in variable1

#can use na.omit() function
mean(na.omit(dat$variable1)) # reduces the number of values to 3, so mean use 3 denominator
#6+12+3 = 21/3 = 7

#is.na looks at all elements in the frame and returns TRUE/FALSE
#covers all rows, all cols
is.na(dat)

#anyNA returns a single TRUE/FALSE to indicate if an NA is found anywhere
anyNA(dat)
anyNA(dat$variable1)


#COMPLETE CASES
complete.cases(dat) #one test per row - is the row NA-free?

#removing the incomplete cases/rows
dat[complete.cases(dat), ]

#IMPUTE MISSINGS with
library(imputeMissings)

dat
imputedVersion <- impute(dat) #will use the median (for numeric) and mode (for factors/chars) by default
imputedVersion <- imputedVersion[, 1:2]
imputedVersion

###################### SCALING/STANDARDIZING ###############################
# will involve computing the mean/standard deviation
# then each value of the scaled variable is scaled by subtracting the mean
# and dividing by the standard deviation
# z = (one_x_value - mean_of_x_variable)/stddev_of_x_variable

scaledVersion <- scale(imputedVersion)

imputedVersion
scaledVersion

################ writing objects to files ##################################

save(imputedVersion, file="d:\\trashtest.Rdata")  #saves object in R format

#as text file
write.table(imputedVersion, file="d:\\trashtest.txt", row=FALSE, sep=",", quote=FALSE)

#as csv file
write.csv(imputedVersion, file="d:\\trashtest.csv", row.names=FALSE, quote=TRUE)



############################ DESCRIPTIVE STATISTICS ###################
############################ DESCRIPTIVE STATISTICS ###################
############################ DESCRIPTIVE STATISTICS ###################
############################ DESCRIPTIVE STATISTICS ###################

#summarize, describe, present
#Measures of location  - central tendency
#Measures of dispersion - spread

dat <- iris

head (dat)

str(dat)

#min and max and range

min(dat$Sepal.Length)
max(dat$Sepal.Length)
rng <- range(dat$Sepal.Length) # an object with the min and max
rng
minval <- rng[1]
maxval <- rng[2]

minval
maxval

rangeval <- maxval - minval

rangeval

#mean, median

mean(dat$Sepal.Length, na.rm = TRUE)

median(dat$Sepal.Length)
quantile(dat$Sepal.Length, 0.5)

#first and third quartiles, IQR

quantile(dat$Sepal.Length, 0.25)

quantile(dat$Sepal.Length, 0.75)

IQR(dat$Sepal.Length)

#standard deviation and variance

sd(dat$Sepal.Length)
var(dat$Sepal.Length)
(sd(dat$Sepal.Length))^2

#apply the same function to a number of variables at the same time:
lapply(dat[, 1:4], sd)
lapply

#summary function - min, max, 1 and 3 quartiles, median, mean
summary(dat)

#grouped summaries
#data frame, what to group by, what function to apply
by (dat, dat$Species, summary)

#pastecs package computes a lot of statistics for a dataframe
library(pastecs)

stat.desc(dat)

#get skew, kurtosis, and normality test with norm=TRUE
#gets standard error of tghe mean, 95% ci for mean,
#coefficient of variation (the standard deviation divided by the mean)
stat.desc(dat, norm = TRUE)

coeff.of.variation <- sd(dat$Sepal.Length)/mean(dat$Sepal.Length)

coeff.of.variation


#MODE
#no specific function for MODE in r, so write your own:
table(dat$Sepal.Length) # TABLE makes a frequency distribution

sort(table(dat$Sepal.Length), decreasing=TRUE)


#CONTINGENCY TABLE - TWO ways
# categorical data
#two discrete variables, combinations of all values, with counts

#converting sepal length to qualitative/categorical
#add as a NEW column to dat dataframe
dat$size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length), "small", "big")

table(dat$size) # just frequencies of the two size values just created

#frequencies of the value combinations of the two categoricals
#cases
table(dat$Species, dat$size)
xtabs(~ dat$Species + dat$size) #can switch order to switch rows/cols

#relative frequencies - add up all cells to see 100%
prop.table(table(dat$Species, dat$size))

#percentages by row (1)
prop.table(table(dat$Species, dat$size), margin = 1)
#percentages by column(2)
prop.table(table(dat$Species, dat$size), margin = 2)
