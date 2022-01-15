
##################################### CHI-SQUARE GOODNESS OF FIT TEST ################
#The chi-square goodness of fit test is used to compare the observed distribution 
#to an expected distribution, in a situation where we have two or 
#more categories/levels in a discrete data. In other words, it compares multiple 
#observed proportions to expected probabilities. 

#####################################################################

#Collecting wild tulips, find 81 are red, 50 are yellow, and 27 are white
#Could ask: Are the colors equally distributed?  1/3 of each color?
#could ask: What we were expecting is below, so is there a significant
#difference between observed color proportions and the expected proportions?
#     red:yellow:white to be 3:2:1, so 1/2 should be red
#     1/3 should be yellow, 1/6 should be white

#NULL hypothesis: there is no significant difference between observed and expected
#ALTERNATIVE hypothesis: there is a significant diff between observed and expected

################# Test : are the colors equally common #UNIFORM DISTRIBUTION 
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

###################Test : are the proportions 3:2:1?    #RANDOM DISTRIBUTION

tulips.observed <- c(81, 50, 27)    #(red, yellow, white)
tulips.expectproportions <- c(3/6, 2/6, 1/6) #red:yellow:white

test <- chisq.test(tulips.observed, p=tulips.expectproportions)
test

#The observed proportions  are NOT significantly different from the expected
#proportions.
test$p.value
# check that the expected values are all ABOVE FIVE
test$expected

##################Test : #BINOMIAL DISTRIBUTION ######################

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

####################### EXAMPLE: Normal Distribution from Daniel & Cross Text pp. 604ff

# assess short-term clinical, economic, humanistic outcomes of pharmaceutical care services
# for patients with diabetes in community pharmacies.

# DATA SET:   47 subjects with subject counts by cholesterol range
# QUESTION:   Does the data provide sufficient evidence to indicate that the cholesterol levels sample 
#             DID NOT come from a normally distributed population with alpha = 0.05?
# ASSUMPIONS: Simple random sample has been supplied
# H0 (NULL):  In the population from which the sample was drawn, cholesterol levels are normally
#             distributed
# HA (ALT):   The sample is NOT normally distributed

dat <- data.frame(
  "lower" = c(100, 125, 150, 175, 200, 225, 250, 275),
  "upper" = c(124.9, 149.9, 174.9, 199.9, 224.9, 249.9, 274.9, 299.9),
  "Observed.Frequency" = c(1, 3, 8, 18, 6, 4, 4, 3)
)

z.scores = (dat$lower - 198.65)/41.31

pnorm(1.85, mean=0, sd=1) - pnorm(1.24, mean=0, sd=1)

pnorm(0.03, mean=0, sd=1) - pnorm(-0.57, mean=0, sd=1)






snorm(z.scores, 198.65, 41.31)

pnorm(200, 198.65, 41.31, lower.tail = TRUE)



1-pnorm(250, 198.65, 41.31, lower.tail = TRUE)
pnorm(124.9, 198.65, 41.31, lower.tail = TRUE)
pnorm(300, 198.65, 41.31, lower.tail=FALSE)

