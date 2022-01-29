#ANOVA EXAMPLES

#ANOVA Checklist
##### dependent/outcome variable: 
#####      a continuous variable (measurements/observed #s)
##### independent variable:
#####      qualitative/categorical with at least 2 levels/groups
#  1. simple random sample/observations
#  2. independence of observations within groups
#  3. independence of the groups
#  4. Sample size, each group large enough (> 20) or normal
#  5. variation/standard deviation of groups roughly equal

#EXample 1 from R and Stats Blog
#Uses a penguin dataset
#https://statsandr.com/blog/anova-in-r/#data

#install.packages("palmerpenguins")
#install.packages("hrbrthemes")
#install.packages("tidyverse")
#install.packages("viridis")
#install.packages("forcats")
#install.packages("report")
#install.packages("multcomp")



library(palmerpenguins)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(forcats)
library(dplyr)
library(car)
library(lattice)
library(report)
library(multcomp)

penguins

#keep only these columns in your copy of the dataset

dat <- penguins %>%
  select(species, flipper_length_mm)

#Summarize entire dataset
summary(dat)

#summarize flipper length by species
aggregate(flipper_length_mm ~ species,
          data = dat,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)

#summarize using dplyr
group_by(dat, species) %>%
  summarise(
    mean = mean(flipper_length_mm, na.rm = TRUE),
    sd = sd(flipper_length_mm, na.rm = TRUE)
  )

#jitter plot for flipper length grouped by species
ggplot(dat) +
  aes(x=species, y=flipper_length_mm, color=species) +
  geom_jitter() +
  theme(legend.position = "none")


boxplot(flipper_length_mm ~ species,
        data = dat
)

#Assumptions for penguins
#assuming simple random sample and penguins cannot be more
#than one species (obviously)
#only one measurement per penguin, species groups randomly
#selected
#three distinct categories for the flipper length measurements
#Normality:number of observations in each group > 30, CLT covers it.
#In ANOVA you actually have two options for testing normality:
#   Checking normality separately for each group on the “raw” data (Y values)
#   Checking normality on all residuals (but not per group)
#   In practice, you will see that it is often easier to just use the 
#     residuals and check them all together, especially 
#     if you have many groups or few observations per group.



#https://cran.r-project.org/web/packages/forcats/vignettes/forcats.html
#https://www.r-graph-gallery.com/histogram_several_group.html

#mirror histograms - each species mapped atop each other

p <- dat %>%
  ggplot(aes(x=flipper_length_mm, fill=species)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity')+
  scale_fill_manual(values=c("#69b3a2", "#404080", "#A31544")) +
  theme_ipsum() +
  labs(fill="")

p

#entirely separate histograms

p <- dat %>%
  mutate(text = fct_reorder(species, flipper_length_mm)) %>%
  ggplot(aes(x=flipper_length_mm, fill=species)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~text)

p

#QQ plots for each species

dat %>%
  ggplot(aes(sample = flipper_length_mm)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~species)

############# GRAPHING NORMALITY OF RESIDUALS #######

residuals_aov <- 
    aov(flipper_length_mm ~ species,
        data = dat
    )


par(mfrow = c(1, 2)) # combine plots

# histogram
hist(residuals_aov$residuals)

# QQ-plot

qqPlot(residuals_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)


############# testing residuals for normality with SHAPIRO-WILK

shapiro.test(residuals_aov$residuals)
#we do not reject the hypothesis that the residuals
#follow a normal distribution


#TESTING EQUAL VARIANCE ACROSS ALL GROUPS - BOXPLOT, DOTPLOT
#LEVENE's test

boxplot(flipper_length_mm ~ species,
        data = dat
)


dotplot(flipper_length_mm ~ species,
        data = dat
)

leveneTest(flipper_length_mm ~ species, data=dat)

#The levene's test's p value is > 0.05, so we do not reject the NULL
#hypothesis and we cannot reject the hypothesis that variances are equal
#between species.


#another way to test normality and 
#homogeneity of variances visually all at once:

#if the red line is not pretty flat, homogeneity of
#variances is violated.

par(mfrow = c(1, 2)) # combine plots

plot(residuals_aov, which = 3)
plot(residuals_aov, which = 2)


############################## ANOVA TEST #################################
############################## ANOVA TEST #################################
############################## ANOVA TEST #################################
############################## ANOVA TEST #################################

#method #1: easy to switch the var.equal to FALSE to perform WELCH's ANOVA
oneway.test(flipper_length_mm ~ species, data=dat, var.equal=TRUE)

#method #2: requires two steps but  saves a full ANOVA table to the
#results variable, which can be used in the post-hoc tests

residuals_anova <- aov(flipper_length_mm ~ species, data=dat)
summary(residuals_anova)

#And you can get a nice English report of the results;
report(residuals_anova)

#Rejecting the NULL since p is very small, we know only that at least
#one group is different.  Answering which groups are different
#requires  a post-ho test or multiple pairwise comparisons

############################## POSTHOC TESTS ##################################
############################## POSTHOC TESTS ##################################
############################## POSTHOC TESTS ##################################
############################## POSTHOC TESTS ##################################


#Tukey HSD:
#When you have no particular referernce group - you want to compare all groups 
#to find the difference
#look at table labeled Linear Hypotheses; first and last columns, which show the
#two groups compared and their adjusted p-values.  The NULL hypothesis is that
#the two groups are equal/do not differ.
#The entire set of comparisons has an error rate of 0.05

post_test_tukey_hsd <- glht(residuals_anova, linfct=mcp(species="Tukey"))

summary(post_test_tukey_hsd)

#The results of this test are that all groups are  significantly different.
#The NULL hypothesis is rejected in all three pairs of tests.

#this plot shows the 95% confidence interals, none crossing the ZERO
#line
plot(mar=c(3,8,3,3))
plot(post_test_tukey_hsd)

#another way to perform Tukey's HSD

#the rightmost columns p-adj
TukeyHSD(residuals_anova)

plot(TukeyHSD(residuals_anova))
