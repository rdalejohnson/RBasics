set.seed(42)

my_vec <- rnorm(10, mean = 400, sd = 10)

plot(my_vec,
     type = "l", # "l" stands for line
     main = "Plot title",
     ylab = "Y-axis label",
     xlab = "X-axis label"
)

# THE SIX BASIC DATA TYPES
  # Numeric
  # Integer
  # Complex
  # Character
  # Factor
  # Logical

 
#Numeric #####################################
num_data <- c(3, 7, 2)
class(num_data)

num_data_dec <- c(3.4, 7.1, 2.9)
class(num_data_dec)


#Integer #####################################

children <- c(3, 5, 7, 2, 0, -1)
class(children)

children <- as.integer(c(3, 5, 7, 2, 0, -1))
class(children)


#character  ##################################
chars <- "test value"
class(chars)

otherchars <- as.character(children)

#entire vectors become character vectors 
intvector <- c(1,3,5,7,9,-3)
class(intvector) #numeric at first
intvector <-   c(intvector, 99, 101, 301, "charlier")

intvector

#FACTORS ########################################

#use factors for character data with a limited number of discrete values
gender <- factor(c("male", "female", "female", "female", "male", "male"))

#levels returns the distinct values in a vector of factors
levels(gender)

class(gender)

#convert a vector of character strings to a factor vector

jobTitles <- c("manager", "front-line worker", "manager", "manager", "driver", "driver", "front-line worker")
jobTitleAsFactors <- as.factor(jobTitles)

levels(jobTitleAsFactors)


#LOGICAL ##########################################

logicals <- c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, 3<5)
logicalsAsInts  <- as.numeric(logicals)

logicalsAsInts

intsAsLogical <- as.logical(logicalsAsInts)
intsAsLogical


