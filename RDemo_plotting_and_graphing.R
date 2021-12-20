install.packages("vcd")

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


############### BAR plot : visualize distribution of a qualitative variable  ##

barplot(table(dat$size))  #frequencies are plotted

barplot(prop.table(table(dat$size))) #proportions/relative frequencies
  
library(ggplot2)

ggplot(dat) +
  aes(x = size) +
  geom_bar()