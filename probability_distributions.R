# Probability Distributions of Discrete Random Variables
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

#probabliity that a randomly selected family used 5 or more programs?
freqTable %>% filter(between(numberFoodAssistPrograms, 1, 4)) %>% 
  summarize(total = 1 - sum(probability))

#probability of randomly selected family using between 3 and 5 programs inclusive
freqTable %>% filter(between(numberFoodAssistPrograms, 1, 5)) %>% 
  summarize(total = sum(probability))

freqTable %>% filter(between(numberFoodAssistPrograms, 1, 2)) %>% 
  summarize(total = sum(probability))

