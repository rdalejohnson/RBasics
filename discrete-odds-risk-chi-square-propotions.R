################## VARIOUS CHI-SQUARE TESTS/ PROPORTION TESTS / ODDS RATIO, RISK RATIO, RISK DIFFERENCES

###################### "BY HAND" example ###############################
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


########################## END OF BY-HAND EXAMPLE ##############################


##################### one-proportion test using r functions #########
# https://statsandr.com/blog/one-proportion-and-goodness-of-fit-test-in-r-and-by-hand/#fnref4

dat <- iris

dat$size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length),
                   "small", "big")

head(dat, n=5)


############## ONE PROPORTION TEST ###############

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

##################### Proportion test  setup #########################
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
