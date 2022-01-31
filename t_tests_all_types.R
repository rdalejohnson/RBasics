#### T-tests

install.packages('Rcpp')
library(Rcpp)

library(ggplot2)
library(ggstatsplot)


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
