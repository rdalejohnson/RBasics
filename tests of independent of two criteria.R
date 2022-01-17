#Tests of Independence
#Daniel and Cross pp 619ff
#https://statsandr.com/blog/chi-square-test-of-independence-in-r/

#Chi-square is a nonparametric test:
#    Requires no assumptions about population
#    parameters and perform on test hypotheses
#    about the population parameters.

#The chi-square test of independence can be used to test the NULL hypothesis
#that two criteria of classification, applied to the same set of entities, are
#independent. - that is, the distribution of one criterion is the same no matter
#what the distribution of the other criterion.

#The NULL hypothesis is always that the two criteria of 
#classification are independent - there is NO relationship between the two 
#categorical variables; knowing the value of one variable does not help predict
#the value of the other variable.
#If the NULL is rejected, we conclude that the two criteria are NOT independent - 
#there IS a relationship between the two categorical variables, knowing the value
#of one variable helps predict the value of the other variable.

#Example: if socioeconomic status and area of residence of inhabitants in a city
#         are independent, we expect to find the same proportion of families in the
#         low, medium, and high socioeconomic groups in all areas of the city

#Typically involves a contingency table with counts/frequencies

#Given the observed counts from a sample, compute what the expected counts would
#be if the NULL hypothesis is true and the two criteria are independent.
#If the two are independent, the probability of their joint occurrence is equal
#to the product of their individual probabilities.

#Example from Daniel and Cross, pg 621ff
#### MAKING A table out of frequency counts:

race.folic.acid <- matrix(c(260, 299, 15, 41, 7, 14), ncol=2, byrow=TRUE)
colnames(race.folic.acid) <- c("Yes", "No")
rownames(race.folic.acid) <- c("White", "Black", "Other")

race.folic.acid <- as.table(race.folic.acid)

race.folic.acid

barplot(race.folic.acid, legend=T, beside=FALSE, main= "Folic Acid Use by Race")


test <- chisq.test(race.folic.acid)
test

test$method
test$observed
test$expected

####################### RESIDUALS ############################

#The larger the residual, the greater the contribution of the cell to the magnitude of the
#resulting chi-square obtained value. 

#https://scholarworks.umass.edu/cgi/viewcontent.cgi?article=1269&context=pare
#cells with the largest expected values also produce the largest raw residuals. To
#overcome that redundancy, a standardized or Pearson
#residual is calculated by dividing the raw residual by the
#square root of the expected value as an estimate of the
#raw residual’s standard deviation:

#The unstandardized/raw residual is the simple difference of the
#observed and expected values. Unstandardized residual = O - E

# RESIDUALS produced by chisq.test are computed as: 
#(observed - expected) / sqrt(expected).
test$residuals

# STANDARDIZED RESIDUALS are computed as  (observed - expected) / sqrt(V)
# where V is the residual cell variance
test$stdres

#Adjusted standardized residuals are standardized residuals that are adjusted for the
#row and column totals. The adjusted standardized residual is defined as:
#Adjusted standardized residual = O - E / SQRT[nA * nB * (1 - nA/N) * (1 - nB/N) / N]

##########################################################
#EXample from https://rcompanion.org/rcompanion/b_05.html
#R companion size to Handbook for biological statistics


if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(grid)){install.packages("grid")}
if(!require(pwr)){install.packages("pwr")}

#Building a matrix from a text block:

Input =("
Injection.area  No.severe  Severe
Thigh           4788       30
Arm             8916       76
")

Matriz = as.matrix(read.table(textConnection(Input),
                   header=TRUE,
                   row.names = 1))

Matriz

chisq.test(Matriz, correct=TRUE)


#Building a matrix from a set of vectors:

R1 = c(4788, 30)
R2 = c(8916, 76)
rows = 2
Matriz = matrix(c(R1,R2), nrow=rows, byrow=TRUE)

#giving rows and columns names:

rownames(Matriz) = c("Thigh", "Arm")
colnames(Matriz) = c("No.severe", "Severe")

chisq.test(Matriz, correct=TRUE)


###########################################################
#post-hoc test for chi-square of independence

Input = ("
         Supplement  No.cancer Cancer
         'Selenium'    8177       575
         'Vitamin E'   8117       620
         'Selenium+E'  8147       555
         'Placebo'     8167       529
         ")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names = 1))

Matriz

chisq.test(Matriz)


#############PAIRWISE TESTS ################
##https://alanarnholt.github.io/PDS-Bookdown2/post-hoc-tests-1.html
##https://rcompanion.org/rcompanion/b_05.html

pairz1 = matrix(c(Matriz[1,1], Matriz[1,2], Matriz[2,1], Matriz[2,2]),
                nrow=2, byrow=TRUE)
rownames(pairz1) = c(rownames(Matriz)[1], rownames(Matriz)[2])  
colnames(pairz1) = c(colnames(Matriz)[1], colnames(Matriz)[2])

pairz1
chisq.test(pairz1)

#####

pairz2 = matrix(c(Matriz[1,1], Matriz[1,2], Matriz[3,1], Matriz[3,2]),
                nrow=2, byrow=TRUE)
rownames(pairz2) = c(rownames(Matriz)[1], rownames(Matriz)[3])  
colnames(pairz2) = c(colnames(Matriz)[1], colnames(Matriz)[2])

pairz2
chisq.test(pairz2)


#####

pairz3 = matrix(c(Matriz[1,1], Matriz[1,2], Matriz[4,1], Matriz[4,2]),
                nrow=2, byrow=TRUE)
rownames(pairz3) = c(rownames(Matriz)[1], rownames(Matriz)[4])  
colnames(pairz3) = c(colnames(Matriz)[1], colnames(Matriz)[2])

pairz3
chisq.test(pairz3)


#####

pairz4 = matrix(c(Matriz[2,1], Matriz[2,2], Matriz[3,1], Matriz[3,2]),
                nrow=2, byrow=TRUE)
rownames(pairz4) = c(rownames(Matriz)[2], rownames(Matriz)[3])  
colnames(pairz4) = c(colnames(Matriz)[1], colnames(Matriz)[2])

pairz4
chisq.test(pairz4)

#####  THIS IS THE ONLY SIGNIFICANT DIFFERENCE of all the pairs

pairz5 = matrix(c(Matriz[2,1], Matriz[2,2], Matriz[4,1], Matriz[4,2]),
                nrow=2, byrow=TRUE)
rownames(pairz5) = c(rownames(Matriz)[2], rownames(Matriz)[4])  
colnames(pairz5) = c(colnames(Matriz)[1], colnames(Matriz)[2])

pairz5
chisq.test(pairz5)


#####

pairz6 = matrix(c(Matriz[3,1], Matriz[3,2], Matriz[4,1], Matriz[4,2]),
                nrow=2, byrow=TRUE)
rownames(pairz6) = c(rownames(Matriz)[3], rownames(Matriz)[4])  
colnames(pairz6) = c(colnames(Matriz)[1], colnames(Matriz)[2])

pairz6
chisq.test(pairz6)



############ BAR CHARTS ####################


Input = ("
         Supplement  No.cancer Cancer
         'Selenium'    8177       575
         'Vitamin E'   8117       620
         'Selenium+E'  8147       555
         'Placebo'     8167       529
         ")

Prostate = read.table(textConnection(Input), header=TRUE)

library(dplyr)

Prostate

# add row totals
Prostate =
  mutate(Prostate,
         Sum = No.cancer + Cancer)

Prostate

# add proportions, lower and upper confidence intervals for a proportion

# CI for a proportion uses the binomial distribution/binom test

#The APPLY family of functions manipulates slices of data from mamtrices, arrays, lists,
#and dataframes
#margin = 1 means apply the function over rows

Prostate =
  mutate(Prostate,
         Prop = Cancer / Sum,
         low.ci = apply(Prostate[c("Cancer", "Sum")], 1,
                        function(y) binom.test(y['Cancer'], y['Sum'])$ conf.int[1]),
         high.ci = apply(Prostate[c("Cancer", "Sum")], 1,
                         function(y) binom.test(y['Cancer'], y['Sum'])$ conf.int[2])
  )

Prostate



### Plot (Bar chart plot)

library(ggplot2)

ggplot(Prostate,
       aes(x=Supplement, y=Prop)) +
  geom_bar(stat="identity", fill="gray40",
           colour="black", size=0.5,
           width=0.7) +
  geom_errorbar(aes(ymax=high.ci, ymin=low.ci),
                width=0.2, size=0.5, color="black") +
  xlab("Supplement") +
  ylab("Prostate cancer proportion") +
  scale_x_discrete(labels=c("Selenium", "Vitamin E",
                            "Selenium+E","Placebo")) +
  ## ggtitle("Main title") +
  theme(axis.title=element_text(size=14, color="black",
                                face="bold", vjust=3)) +
  theme(axis.text = element_text(size=12, color = "gray25",
                                 face="bold")) +
  theme(axis.title.y = element_text(vjust= 1.8)) +
  theme(axis.title.x = element_text(vjust= -0.5))


