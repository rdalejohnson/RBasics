# install.packages("vcd")
# install.packages("car")

# PLOTTING/GRAPHING 

dat <- iris
dat$size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length), "small", "big")

################# MOSAIC plot : categorical data #######################

#visualize contingency table of two qualitative/categoricals

#table() creates the contingency table

table(dat$Species, dat$size)

mosaicplot(table(dat$Species, dat$size),
           color = TRUE,
           xlab = "Species", # label for x-axis
           ylab = "Size" # label for y-axis
)

library(vcd)  #visualizing categorical data

mosaic(~ Species + size, data = dat, 
       direction= c("v", "h"))

# https://r-graphics.org/recipe-miscgraph-mosaic

#different orientations (vertical, horizontal) change
#how we see the data.

UCBAdmissions
ftable(UCBAdmissions) #flat contingency tables

mosaic( ~ Admit + Gender + Dept, data = UCBAdmissions)

mosaic( ~ Dept + Gender + Admit, data = UCBAdmissions,
        highlighting = "Admit", highlighting_fill = c("lightblue", "pink"),
        direction = c("v","h","v"))


mosaic( ~ Dept + Gender + Admit, data = UCBAdmissions,
        highlighting = "Admit", highlighting_fill = c("lightblue", "pink"),
        direction = c("v", "v", "h"))

############### BAR plot : visualize distribution of a QUALITATIVE variable  ##
### nominal and ordinal variables

barplot(table(dat$size))  #frequencies are plotted

barplot(prop.table(table(dat$size))) #proportions/relative frequencies
  
library(ggplot2)

ggplot(dat) +
  aes(x = size) +
  geom_bar()


############# HISTOGRAM - distribution of a quantitative variable
#break the range of the variable into intervals,
#count #observations in each interval
#quantitative (histogram) vs. qualitative (barplots)

hist(dat$Sepal.Length)

#rule of thumb, Sturges' law, number of bins should
#be rounded value of the square root of the # of
#observations
#Example: 150 observations, sqrt = 12.24745, so 12 bins

ggplot(dat) +
  aes(x = Sepal.Length) +
  geom_histogram(bins=12)


############## BOX PLOT: distribution of a quantitative variable
#displays the five common location summary stats: min, max, median,
#1st/3rd quantiles, then outliers based on the IQR

#single boxplot
boxplot(dat$Sepal.Length)

#sepal length all 3 species
boxplot(dat$Sepal.Length ~ dat$Species)

ggplot(dat) +
  aes(x = Species, y = Sepal.Length) +
  geom_boxplot()


############### DOT PLOT: distribution of quantitative variable ################
# like a box plot but all values are points on a

library(lattice)

dotplot(dat$Sepal.Length ~ dat$Species)


############# SCATTERPLOT - often used to visualize
# potential correlation between two quantitative variables

plot(dat$Sepal.Length, dat$Petal.Length)

ggplot(dat) +
  aes(x = Sepal.Length, y = Petal.Length) +
  geom_point()

#with color:
ggplot(dat) +
  aes(x = Sepal.Length, y = Petal.Length, colour = Species) +
  geom_point() +
  scale_color_hue()


######## LINE PLOTS - not so useful with this example but great for time series
plot(dat$Sepal.Length,
     type = "l"
) # "l" for line


#### QQ PLOTS - checking the normality assumptions of a quantitative
#### variable (histograms are the other way)

qqnorm(dat$Sepal.Length)
# Draw the reference line:
qqline(dat$Sepal.Length)

library(carData)
library(car)

qqPlot(dat$Sepal.Length)
#need car/carData to get the confidence bands into
#the qqplot
#if points are close to the reference line and within
#the confidence bands, the normality assumption
#is met.
#The sepal.length variable is not normally distributed
#since points lie outside the bands.

# QQ plots for normality in groups
# gives two plots, one for  big and one for small
qqPlot(dat$Sepal.Length, groups = dat$size)

library(ggplot2)
#both on a single plot
qplot(
  sample = Sepal.Length, data = dat,
  col = size, shape = size
)

#############density plot #########
#like a smooth histogram, represents the distribution of a numeric variable
#functions PLOT and DENSITY are used together

density(dat$Sepal.Length)

plot(density(dat$Sepal.Length))

ggplot(dat) +
  aes(x = Sepal.Length) +
  geom_density()


