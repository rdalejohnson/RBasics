#Tests of Independence
#Daniel and Cross pp 619ff
#https://statsandr.com/blog/chi-square-test-of-independence-in-r/

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
