################## VARIOUS CHI-SQUARE TESTS/ PROPORTION TESTS / ODDS RATIO, RISK RATIO, RISK DIFFERENCES

# https://stats.stackexchange.com/questions/2391/what-is-the-relationship-between-a-chi-squared-test-and-test-of-equal-proportion
# prop.test is a  z-test; lets you test 
#    whether proportions are comparable between groups
#    whether a proportion differs from theoretical probabilities
# a chi-square test for equality of two proportions is EXACTLY the same thing as a z-test
# since the chi-square distribution with one degree of freedom is the normal deviate but squared.

# a one-sample proportion test assesses if a population proportion is significantly different
# from a hypothesized value (P0) - the hypothesis of inequality

# http://www.sthda.com/english/wiki/one-proportion-z-test-in-r
# R functions binom.test() and prop.test() can be used to perform the one-proportion test:
# binom.test is recommended when the sample size is small - the "exact binomial test"
# prop.test() can be used when sample size is larger than 30 as it is a normal approximation to the
# binomial

#By default, prop.test() uses the Yates Continuity correction.  Setting Yates to FALSE
#makes the test equivalent to the uncorrected z-test of a proportion

###################### One-sample proportion test "BY HAND" example #1 ###############################

#LEAD levels in blood changes incidence of colic in children.

#https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/sas/sas6-categoricaldata/SAS6-CategoricalData2.html

#total number of children in study: 124
#total number of children with colic: 23
#general public estimate to be used: 7% of children have colic

#NULL hypothesis: proportion of colic among children 
#                 living near lead smelters is 0.07
#ALTERNATIVE hypothesis: proportion of colic among children
#                        living near lead smelters is not 0.07

number.observations = 124
number.colic = 23
p.hat = round(number.colic/number.observations, 2)
p.population = 0.07

#want a 95% confidence interval, so alpha = 0.05, so z cutoff is +/- 1.96
z.score.cuttoff.95 = 1.96

p.hat

#calculate the test statistic assuming a standard normal distribution


z.score = (p.hat - p.population)/
          sqrt((p.population * (1-p.population))/number.observations)

z.score

#z.score is well above 1.96 so we REJECT the NULL;
#the prevalence of colic is significantly different from 0.07.

#compute the confidence intervals for the statistic:

lower.bound.CI = p.hat - z.score.cuttoff.95 * sqrt((p.hat*(1-p.hat))/number.observations)
upper.bound.CI = p.hat + z.score.cuttoff.95 * sqrt((p.hat*(1-p.hat))/number.observations)


lower.bound.CI
upper.bound.CI


########################## END OF BY-HAND EXAMPLE 1 ##############################


###################### One-sample proportion test "BY HAND" example #2 ###############################

# coin toss example
# a coin is tossed 100 times and comes up heads 67 times
# is the coin fair?
# NULL hypothesis: proportion of heads is 50%
# ALTERNATIVE hypothesis: proportion of heads if NOT 50%

number.observations = 100
number.heads = 67
p.hat = round(number.heads/number.observations, 2)
p.population = 0.50

#want a 95% confidence interval, so alpha = 0.05, so z cutoff is +/- 1.96
z.score.cuttoff.95 = 1.96

p.hat

#calculate the test statistic assuming a standard normal distribution

z.score = (p.hat - p.population)/
  sqrt((p.population * (1-p.population))/number.observations)

z.score

z.score*pnorm(-abs(z.score))

#z.score is well above 1.96 so we REJECT the NULL;
#the coin is not fair - number of heads is significantly different from 0.5.

#compute the confidence interval upper/lower for the statistic:

lower.bound.CI = p.hat - z.score.cuttoff.95 * sqrt((p.hat*(1-p.hat))/number.observations)
upper.bound.CI = p.hat + z.score.cuttoff.95 * sqrt((p.hat*(1-p.hat))/number.observations)


lower.bound.CI
upper.bound.CI


# http://www.sthda.com/english/wiki/one-proportion-z-test-in-r



##################### one-proportion test using r functions #########
# https://statsandr.com/blog/one-proportion-and-goodness-of-fit-test-in-r-and-by-hand/#fnref4

##############################################################################
############## ONE PROPORTION TEST, FLOWER SIZE example, R FUNCTIONS ###############

dat <- iris

dat$size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length),
                   "small", "big")

head(dat, n=5)



library (ggplot2)

#look at frequencies, proportions in the data set:

ggplot(dat) + 
  aes(x=size) +
  geom_bar(fill="#5c8c4a") +
  theme_minimal()

#counts
table(dat$size)
nrow(dat)

#proportions of the total sample, rounded to 2 decimal places
round(prop.table(table(dat$size)), 2)

##################### One-Proportion test  setup #########################
# treat one of the big/small categories as "success"
# H0: proportions of big and small flowers are equal (so both would be .5 = 50%)
# H1: proportions of big and small flowers are NOT equal


#The ONE PROPORTION FUNCTION/METHOD in R is:

#nrow(dat[dat$size == "big",])
length(dat$size[dat$size == "big"])

test <- prop.test(
          x = length(dat$size[dat$size == "big"]), # 77, number of successes/big flowers
          n = nrow(dat), # 150, total number of "trials"
          p = 0.5,  # the proportions are both 50%
          conf.level = 0.95
)

test

# this test DOES NOT reject the null with a p value of 0.8065
# so the proportions are the same


##################################################################################
####### Fair coin example from above done with R function

number.observations = 100
number.heads = 67
p.hat = round(number.heads/number.observations, 2)
p.population = 0.50

test <- prop.test(
  x = number.heads,
  n = number.observations,
  p = 0.50,  # the proportions are both 50%
  conf.level = 0.95
)

test

##############################
# https://stats.stackexchange.com/questions/2391/what-is-the-relationship-between-a-chi-squared-test-and-test-of-equal-proportion
# SHOWING that the chi-square test with one degree of freedom
# and the two-sample equality of proportions ARE THE SAME:

tab <- matrix(c(100, 80, 20, 10), ncol = 2)

tab

chisq.test(tab)

prop.test(tab)



##################################################
# http://www.sthda.com/english/wiki/one-proportion-z-test-in-r

#we have a population of mice containing half male and have female (p = 0.5 = 50%). 
#Some of these mice (n = 160) have developed a spontaneous cancer, 
#including 95 male and 65 female.

#Two-tailed
#NULL Hypothesis: the proportion of males with cancer to females with cancer is equal
#ALTERNATIVE Hypothesis: the propotion of males with cancer differs from the proportion of femals with cancer

number.observations = 160
number.cancer = 95
p.hat = round(number.cancer/number.observations, 2)
p.population = 0.50

test <- prop.test(
  x = number.cancer,
  n = number.observations,
  p = 0.50,  # the proportions are both 50%
  conf.level = 0.95,
  correct = FALSE   #no YATES cc
)

test

test$p.value
test$statistic
test$estimate
test$conf.int

btest <- binom.test(
  x = number.cancer,
  n = number.observations,
  p = 0.50,  # the proportions are both 50%
  conf.level = 0.95,
)

btest

#we REJECT the null; the proportions of males and females with cancer are NOT equal



##################################### CHI-SQUARE GOODNESS OF FIT TEST ################
#The chi-square goodness of fit test is used to compare the observed distribution 
#to an expected distribution, in a situation where we have two or 
#more categories/levels in a discrete data. In other words, it compares multiple 
#observed proportions to expected probabilities. 

################ EXAMPLE 1 using R functions

#Collecting wild tulips, find 81 are red, 50 are yellow, and 27 are white
#Could ask: Are the colors equally distributed?  1/3 of each color?
#could ask: What we were expecting is below, so is there a significant
#difference between observed color proportions and the expected proportions?
#     red:yellow:white to be 3:2:1, so 1/2 should be red
#     1/3 should be yellow, 1/6 should be white

#NULL hypothesis: there is no significant difference between observed and expected
#ALTERNATIVE hypothesis: there is a significant diff between observed and expected

# Test #1: are the colors equally common?
tulips.distribution <- c(81, 50, 27)
test <- chisq.test(tulips.distribution, p=c(1/3, 1/3, 1/3))
test
test$p.value

# conclusion: p-value is below 0.05.
#the observed proportions are significantly different from the
# expected proportions

# check that the expected values are all ABOVE FIVE
test$expected
test$parameter  #get degrees of freedom

#Test #2: are the proportions 3:2:1?
tulips.observed <- c(81, 50, 27)    #(red, yellow, white)
tulips.expectproportions <- c(3/6, 2/6, 1/6) #red:yellow:white

test <- chisq.test(tulips.observed, p=tulips.expectproportions)
test

#The observed proportions  are NOT significantly different from the expected
#proportions.
test$p.value
# check that the expected values are all ABOVE FIVE
test$expected


# https://statsandr.com/blog/one-proportion-and-goodness-of-fit-test-in-r-and-by-hand/#fnref4
# goodness of fit with a particular known distribution

#100 families with 5 children
#Question: Is the number of girls in the families following a binomial distribution?

library(dplyr)

#make a dataframe from scratch:

dat <- data.frame(
  "Girls" = c(0:5), 
  "Observed.Frequency" = c(8, 60, 65, 85, 64, 8)
)

#mutate data frame new column of relative frequency of the observations
dat <- dat %>% 
  mutate(observed_relative_freq = Observed.Frequency/sum(dat$Observed.Frequency))

dat

# Add expected frequencies based on binomial distribution;
# assume a probability of 0.5 for one baby being a girl
# dbinom(quantilesVector, size=numberOfTrials, prob=probability.success.each.trial)

dbinom(0:5, 5, 0.5)


dat <- dat %>% 
  mutate(expected_relative_freq = dbinom(0:5, 5, 0.5))


dat <- dat %>% 
  mutate(Expected.Frequency = expected_relative_freq*sum(dat$Observed.Frequency))

dat

library(ggplot2)

plot1 <- ggplot(dat, aes(x = Girls, y = Expected.Frequency)) +
  geom_bar(stat = "identity", fill = "#F8766D") +
  xlab("Number of girls per family") +
  ylab("Expected frequency") +
  labs(title = "Binomial distribution Bi(x, n = 5, p = 0.5)") +
  theme_minimal()

plot1


test <- chisq.test(dat$Observed.Frequency, p=dat$expected_relative_freq)

test$expected

test$residuals
