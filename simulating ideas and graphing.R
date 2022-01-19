#Simulating many trials and graphing

###############################################################################################
# CHI SQUARE: showing that the mean of the chi-square distribution
# is equal to the degrees of freedom and variance is 
#twice the degrees of freedom
# https://mse.redwoods.edu/darnold/math15/spring2013/R/Activities/ChiSquareTestOfHomogeneity.html

p=0.1047
q=1-p

chi.sq.data=numeric()

M=20000
samp.size=1000

for (i in 1:M) {
  observed=matrix(rep(0,8),nrow=2)
  for (j in 1:4) {
    data = sample(c("Yes","No"),
                  size=samp.size,
                  replace=TRUE,
                  prob=c(p,q))
    observed[1,j]=sum(data=="Yes")
    observed[2,j]=sum(data=="No")
  }
  n=sum(observed)
  
  expected=matrix(rep(0,8),nrow=2)
  for (r in 1:2) {
    for (c in 1:4) {
      expected[r,c]=sum(observed[r,])*sum(observed[,c])/n
    }
  }
  
  chi.sq.data[i]=sum((observed-expected)^2/expected)
  
}

mu=round(mean(chi.sq.data),1)
sigma.sq=round(var(chi.sq.data),1)

hist(chi.sq.data,prob=TRUE,breaks="FD",
     main=paste("mu = ", mu, ", var = ", sigma.sq))
curve(dchisq(x,3),0,max(chi.sq.data),col="red",add=TRUE)

#################################################################################
#https://mse.redwoods.edu/darnold/math15/spring2013/R/Activities/ChiSquareTestOfHomogeneity.html
# showing chi-square computed statistic and where it lives on the
# distribution curve.
# below degrees of freedom is 6; computed chi-square value is 7.486

x=seq(0,20,length=200)
y=dchisq(x,6)
plot(x,y,type="l",col="blue")
xx=seq(7.486,20,length=100)
yy=dchisq(xx,6)
polygon(c(7.486,xx,20),c(0,yy,0),col="red")
arrows(7.486,0.12,7.486,0.09)
text(7.486,0.12,"7.486",pos=3)
