# Probability Distributions of Discrete Random Variables, not from a named distribution
# Data from Chapter 4, Daniel and Cross Biostats, pages 93-98

numberFoodAssistPrograms <- c(1:8)
numberSubject <- c(62, 47, 39, 39, 58, 37, 4, 11)

freqTable <- data.frame(numberFoodAssistPrograms, numberSubject)

#probability/relative frequency of occurence from the count data:
freqTable$probability = round(freqTable$numberSubject/sum(freqTable$numberSubject), 4)

#cumulative probability distribution
freqTable$cumulativeFrequency = cumsum(freqTable$probability)

freqTable

#https://www.youtube.com/watch?v=Q-13s8kLt1w
#OGIVE PLOT
#type: s is stair steps, moves first horizontal, then vertical
#      S is other steps, moves first vertical, then horizontal
plot(freqTable$numberFoodAssistPrograms, 
     freqTable$cumulativeFrequency, 
     main = "Ogive",
     xlab="Number of Programs", 
     ylab="Cumulative Probability",
     type="S")   


library("dplyr")

#probability that a randomly selected family used 5 or more programs?

freqTable %>% filter(between(numberFoodAssistPrograms, 1, 4)) %>% 
  summarize(total = 1 - sum(probability))

#probability of randomly selected family using between 3 and 5 programs inclusive
freqTable %>% filter(between(numberFoodAssistPrograms, 1, 5)) %>% 
  summarize(total = sum(probability)) - 
freqTable %>% filter(between(numberFoodAssistPrograms, 1, 2)) %>% 
  summarize(total = sum(probability))


mean.of.distribution <- sum(freqTable$numberFoodAssistPrograms * freqTable$probability)
#since the probability in the formula above if just the # subjects/total#subject...
mean.of.distribution2 <- sum(freqTable$numberFoodAssistPrograms * round(freqTable$numberSubject/sum(freqTable$numberSubject), 4))

variance.of.distribution <- sum((freqTable$numberFoodAssistPrograms^2 * freqTable$probability)) - mean.of.distribution^2

standard.dev.of.distribution <- sqrt(variance.of.distribution)

#now sample randomly from the probability distribution to test your mean and standard deviation:

sample.of <- sample(x = freqTable$numberFoodAssistPrograms, 
       prob = freqTable$probability,
       size = 10000, 
       replace = TRUE)

mean(sample.of)
sd(sample.of)

########################### BINOMIAL DISTRIBUTION #############################################
