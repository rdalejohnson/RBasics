#### T-tests, Wilcoxon

#install.packages('Rcpp')
#install.packages("psych")
#install.packages("FSA")
#install.packages("lattice")
#install.packages("lsr")
#install.packages("report")

library(Rcpp)
library(ggplot2)
library(ggstatsplot)
library(lattice)




############## One Sample t-test ######################

#https://statsandr.com/blog/how-to-perform-a-one-sample-t-test-by-hand-and-in-r-test-on-one-mean/

#Variance of the population is not known:
#is the population mean larger than 5?

dat2 <- data.frame(
  value = c(7.9, 5.8, 6.3, 7.3, 6.7)
)

#Boxplot shows a mean quite far from 5

ggplot(dat2) +
  aes(y = value) +
  geom_boxplot() +
  theme_minimal()

#one-sided test since we are asking if the mean is larger than 5

test <- t.test(dat2$value,
               mu = 5,  #hypothesized population mean
               alternative = "greater"
)

test$p.value

#p value is well below 0.05, so we can reject the NULL
#hypothesis and there is evidence that the population mean
#is significantly larger than 5.

test$conf.int

#The confidence interval does not cross 1


################ ALTERNATIVE APPROACH using package ggstatsplot and
################ gghistostats() function
###This package and function combines a histogram and the result of
#the statistical test, displayed in the subtitle of the plot

#by default this will  run a two-sided test, which
#will double the p-valu from the "greater only"
#version just above.

gghistostats(
  data = dat2, # dataframe from which variable is to be taken
  x = value, # name of numeric variable whose distribution is of interest
  type = "parametric", # for student's t-test
  test.value = 5 # default value is 0
) +
  labs(caption = NULL) # remove caption


################# two-sample t-tests ####################################
################# two-sample t-tests ####################################
################# two-sample t-tests ####################################

#https://www.youtube.com/watch?v=RlhnNbPZC0A

#compare the means of two samples.  
#Are they drawn from similar or different populations?

# can be used to examine the relationship between a numeric outcome variable Y and
# a categorical explanatory variable X with 2 levels

LungCapData <- read.table(file.choose(), header=T, sep= "\t")
attach(LungCapData)
names(LungCapData)
class(LungCapData)




#############Sodium intake example #########
# https://rcompanion.org/handbook/I_03.html


Input = ("
Instructor       Student  Sodium
'Brendon Small'  a        1200
'Brendon Small'  b        1400
'Brendon Small'  c        1350
'Brendon Small'  d         950
'Brendon Small'  e        1400
'Brendon Small'  f        1150
'Brendon Small'  g        1300
'Brendon Small'  h        1325
'Brendon Small'  i        1425
'Brendon Small'  j        1500
'Brendon Small'  k        1250
'Brendon Small'  l        1150
'Brendon Small'  m         950
'Brendon Small'  n        1150
'Brendon Small'  o        1600
'Brendon Small'  p        1300
'Brendon Small'  q        1050
'Brendon Small'  r        1300
'Brendon Small'  s        1700
'Brendon Small'  t        1300
'Coach McGuirk'  u        1100
'Coach McGuirk'  v        1200
'Coach McGuirk'  w        1250
'Coach McGuirk'  x        1050
'Coach McGuirk'  y        1200
'Coach McGuirk'  z        1250
'Coach McGuirk'  aa       1350
'Coach McGuirk'  ab       1350
'Coach McGuirk'  ac       1325
'Coach McGuirk'  ad       1525
'Coach McGuirk'  ae       1225
'Coach McGuirk'  af       1125
'Coach McGuirk'  ag       1000
'Coach McGuirk'  ah       1125
'Coach McGuirk'  ai       1400
'Coach McGuirk'  aj       1200
'Coach McGuirk'  ak       1150
'Coach McGuirk'  al       1400
'Coach McGuirk'  am       1500
'Coach McGuirk'  an       1200
")

Data = read.table(textConnection(Input),header=TRUE)

library(psych)

headTail(Data)
str(Data)
summary(Data)

#remove the input object now that it is in the dataframe
rm(Input)

library(FSA) #Fisheries Stock Assessment methods/data

#Summarize by Group
FSA::Summarize(Sodium ~ Instructor, data=Data, digits=3)

#Histograms by Group

library(lattice)

lattice::histogram(~ Sodium | Instructor,
          data   = Data,
          type   = "density",
          layout = c(1,2),           ###  columns and rows of individual plots
          
          panel=function(x, ...) {
            panel.histogram(x, ...)
            
            panel.mathdensity(dmath = dnorm,
                              col   = "blue",
                              lwd   = 2,
                              args  = list(mean=mean(x),
                                           sd=sd(x)), ...)})

#boxplots

boxplot(Sodium ~ Instructor, data=Data)


#t-test
t.test(Sodium ~ Instructor, data=Data)

library(lsr)

lsr::cohensD(Sodium ~ Instructor, data=Data)

##############################

#https://statsandr.com/blog/student-s-t-test-in-r-and-by-hand-how-to-compare-two-groups-under-different-scenarios/#scenario-1-independent-samples-with-2-known-variances-1

#two independent samples with 2 equal but unknown variances

dat2 <- data.frame(
  sample1 = c(1.78, 1.5, 0.9, 0.6, 0.8, 1.9),
  sample2 = c(0.8, -0.7, -0.1, 0.4, 0.1, NA)
)

dat2

library(ggplot2)

dat_ggplot <- data.frame(
  value = c(1.78, 1.5, 0.9, 0.6, 0.8, 1.9, 0.8, -0.7, -0.1, 0.4, 0.1),
  sample = c(rep("1", 6), rep("2", 5))
)

ggplot(dat_ggplot) +
  aes(x = sample, y = value) +
  geom_boxplot() +
  theme_minimal()

#This t test assumes UNEQUAL variances, so equal variances
#have to be specified
#one-sided test

test <- t.test(dat2$sample1, dat2$sample2,
               var.equal = FALSE, 
               alternative = "greater"
)

test

#outcome: reject the NULL, the means of the two groups are 
#not the same

#t-test RESULTS using the REPORT package

library("report")

report(test)


#######independent samples with unueqal and unknown variances

dat3 <- data.frame(
  value = c(0.8, 0.7, 0.1, 0.4, 0.1, 1.78, 1.5, 0.9, 0.6, 0.8, 1.9),
  sample = c(rep("1", 5), rep("2", 6))
)
dat3

library(ggplot2)


ggplot(dat3) +
  aes(x = sample, y = value) +
  geom_boxplot() +
  theme_minimal

#one-sided example, var.equal = FALSE indicates use Welch

test <- t.test(value ~ sample, data=dat3, 
               var.equal = FALSE, 
               alternative = "less")

test

############## paired samples, variances of differences is UNKNOWN ##################

dat5 <- data.frame(
  before = c(9, 8, 1, 3, 2),
  after = c(16, 11, 15, 12, 9)
)
dat5


dat5$difference <- dat5$after - dat5$before

ggplot(dat5) +
  aes(y = difference) +
  geom_boxplot() +
  theme_minimal()

#same t.test function but PAIRED-TRUE, one-sided
#the order of the before and after measurements and the alternative option
#should match up

test <- t.test(dat5$after, dat5$before,
               alternative = "greater",
               paired = TRUE
)
test


################ NICE GRAPHICAL OUTPUT with TEST RESULTS #################

# COULD NOT GET THIS ONE TO WORK

library(ggstatsplot)
library(ggplot2)

#paired test, variances of differences is unknown


ggwithinstats(
  data = dat5,
  x = time,
  y = value,
  type = "parametric", # for student's t-test
  centrality.plotting = FALSE # remove mean
) +
  labs(caption = NULL) # remove caption


#WILCOXON SIGNED RANK and Mann-Whitney-Wilcoxon/Mann-Whitney U test

#compares two groups when the normality assumption is violated
#nonparametric
#two versions: 
#samples are independent: Mann-Whitney-Wilcoxon U test
#samples are paired/dependent: Wilcoxon signed-rank

#https://statsandr.com/blog/wilcoxon-test-in-r-how-to-compare-2-groups-under-the-non-normality-assumption/

#independent samples

dat <- data.frame(
  Sex = as.factor(c(rep("Girl", 12), rep("Boy", 12))),
  Grade = c(
    19, 18, 9, 17, 8, 7, 16, 19, 20, 9, 11, 18,
    16, 5, 15, 2, 14, 15, 4, 7, 15, 6, 7, 14
  )
)

dat

################### looking for normality #############

library(ggplot2)

ggplot(dat) +
  aes(x = Sex, y = Grade) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

hist(subset(dat, Sex == "Girl")$Grade,
     main = "Grades for girls",
     xlab = "Grades"
)

hist(subset(dat, Sex == "Boy")$Grade,
     main = "Grades for boys",
     xlab = "Grades"
)


shapiro.test(subset(dat, Sex == "Girl")$Grade)

shapiro.test(subset(dat, Sex == "Boy")$Grade)

#neither data set is normally distributed

#wilcox test
test <- wilcox.test(dat$Grade ~ dat$Sex)
test

#grades between the two groups are significantly different

#using reference levels, this tests for if BOYS have lower
#scores than girls
levels(dat$Sex)

test <- wilcox.test(dat$Grade ~ dat$Sex,
                    alternative = "less"
)
test


#PAIRED TEST######################################


dat2 <- data.frame(
  Beginning = c(16, 5, 15, 2, 14, 15, 4, 7, 15, 6, 7, 14),
  End = c(19, 18, 9, 17, 8, 7, 16, 19, 20, 9, 11, 18)
)

dat2

#convert into a data frame/tidy format
dat2 <- data.frame(
  Time = c(rep("Before", 12), rep("After", 12)),
  Grade = c(dat2$Beginning, dat2$End)
)
dat2

#compute the change between beginning and end measurements

dat2$Time <- factor(dat2$Time,
                    levels = c("Before", "After")
)

ggplot(dat2) +
  aes(x = Time, y = Grade) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#paired wilcox#####

test <- wilcox.test(dat2$Grade ~ dat2$Time,
                    paired = TRUE
)
test

#plotting for independent samples

library(ggstatsplot)

# plot with statistical results
ggbetweenstats( # independent samples
  data = dat,
  x = Sex,
  y = Grade,
  plot.type = "box", # for boxplot
  type = "nonparametric", # for wilcoxon
  centrality.plotting = FALSE # remove median
)

#install.packages("afex")

library(afex)

#plotting for paired samples

ggwithinstats( # paired samples
  data = dat2,
  x = Time,
  y = Grade,
  type = "nonparametric", # for wilcoxon
  centrality.plotting = FALSE # remove median
)


