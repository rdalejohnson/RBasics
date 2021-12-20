# PLOTTING/GRAPHING

dat <- iris
dat$size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length), "small", "big")

########################## MOSAIC plot #######################

#visualize contingency table of two qualitative/categoricals

#table() creates the contingency table

mosaicplot(table(dat$Species, dat$size),
           color = TRUE,
           xlab = "Species", # label for x-axis
           ylab = "Size" # label for y-axis
)