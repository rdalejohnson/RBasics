#Correlation

#https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/

#https://statsandr.com/blog/correlogram-in-r-how-to-highlight-the-most-correlated-variables-in-a-dataset/

#correlation: a measure of the relationship between two variables, are they associated?
#done two variables at a time
#quantitative variables, qualitative ordinal variables
#chi-square is for qualitative nominal variables

#This is linear correlation
#negative correlation: the variables vary in opposite directions
#positive correlation: the variables vary in the same direction
#correlation values range from -1 to 1 
#strong correlations are those closer to -1 or 1.
#a correlation of zero means the variables are independent.
#correlation between X and Y is equal to the correlation between Y and X -
#ORDER DOES NOT MATTER

dat <- mtcars[, c(1, 3:7)]  #could also remove the vs and am categoricals

#cor function - correlation between 2 variables
#Pearson:  the default; for quantitative continuous variables with linear relationship
#Spearman: uses ranks instead of raw data, often used to evaluate
#          relationships with at least one qualitative ordinal or
#          two quantitatives if their link is partially linear.
#Kendall's tau-b: computed from number of concordant pairs, often used for
#          qualitative ordinals

cor(dat$hp, dat$mpg, method="pearson")


#correlation matrix for all possible pairs of variables, rounded to 2 places

round(cor(dat), 2)

#The correlation should be plotted to see if outliers might bias the result.
#Pearson can change drastically with a single point.
#Correlation coefficient will also miss a non-linear relationship - so PLOT.

plot(dat$hp, dat$mpg)

library(ggplot2)

ggplot(dat) +
  aes(x = hp, y = mpg) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()


#PAIRS function - lets you visualize all pairs of variables in a list

pairs(dat[, c("mpg", "hp", "wt")])

#correlation matrix

library(corrplot)

corrplot(cor(dat), method="number", type="upper") #show only upper side


#correlation TEST - are the results statistically significant?










#Convert the correlation matrix into a correlation plot/correlogram/corrgram

#install.packages("corrplot")

#below, a function corrplot2 is created  to make this process all-in-one easier
# data is the name of the dataset
# method: can be pearson, kendall, or spearman; pearson is default
#         PEARSON is best for quantitative continuous;
#         SPEARMAN is best for qualitative ordinal/quantitative with partial linear link
# SIG.LEVEL is the significance level for the correlation test.

#colors: darker the reds, more negative correlation
#        darker the blues, more positive correlation
#        only significant correlations are shown in color; otherws in white

corrplot2 <- function(data,
                      method = "pearson",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficients on the diagonal
           diag = diag
  )
}

corrplot2(
  data = dat,
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)

# ANOTHER WAY TO DO CORRELOGRAMS

#install.packages("ggstatsplot")
#install.packages("ggcorrplot")
library(ggstatsplot)
library(ggcorrplot)

# correlogram with X's over
#non-significant correlations
ggstatsplot::ggcorrmat(
  data = dat,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)


#ANOTHER WAY: Nice horizontal bars showing most relevant correlations in order
#of descending correlation coefficient
# 
#devtools::install_github("laresbernardo/lares")

library(lares)

#Negatives in red/Positives in blue

corr_cross(dat, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 20 # display top X couples of variables (by correlation coefficient)
)

#correlation of A SINGLE VARIALBE 9mpg here) against all others
corr_var(dat, # name of dataset
         mpg, # name of variable to focus on
         top = 5 # display top 5 correlations
)