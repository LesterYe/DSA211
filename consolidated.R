# Consolidated code for statistical learning

#Week 1

data1 <- c(23,25,16,27,25,19,14,18,20,33,36,28,26, 15, 17) # input the data set and call it "data1"
data2 <- c(data1, -10, 33, 44, 59, 67) # input the second data set which includes the first data set plus 5 obs
data3 <- c(1, 4, 14, 18, 28, 15, 28, 28, 37, 21, 11, 10, 45, 64, 74) #  input the third data set

# mean
mean1 <- mean(data1)  # find the mean of the data using function "mean"

# variance
var1 <- var(data1)  # find the sample variance

# Standard Deviation
stdev1 <- sd(data1) 

# Median
median1 <- median(data1) # find the median of data

# 25 percentile
q25 <- quantile(data1, 0.25) # find the 25 percentile

# simple boxplot
boxplot(data1, data2, data3)  # plot the boxplot of three data sets in one graph

# d > pdf/pmf, p > cdf, q > quartile, r > random generator

# Example 2: Binomial distributions n=10, p=0.2
b1 <- dbinom(0:10, 10, 0.2)  
b2 <- pbinom(0:10, 10, 0.2)  
b3 <- qbinom(c(0.25, 0.5, 0.75), 10, 0.2) 
b4 <- rbinom(100, 10, 0.2) 
plot(0:10, b1, col="red", type="l") # plot the pmf

# Example 3: Hyptergeometric distribution
#  Total balls 11, (5 white and 6 black), sample 4 balls without replacement 
# Random variable is the number of white balls in the sample
h1 <- dhyper(0:4, 5, 6, 4)  
h2 <- phyper(0:4, 5, 6, 4)
h3 <- qhyper(c(0.25, 0.5, 0.75, 0.9), 5,6,4)
h4 <- rhyper(20, 5,6,4)
plot(0:4, h1, col="red", type="h")

# Example 4: Poisson Distribution
#  mu=4.25, means the average number of events in the given time frame
p1 <- dpois(0:14, 4.25)
p2 <- ppois(0:14, 4.25)
p3 <- qpois(c(0.25, 0.5, 0.75), 4.25)
p4 <- rpois(35, 4.25)
plot(0:14, p1, col="blue", type="h")

# Example 5: Normal Distribution with mean=4.25, standard deviation=1.5
n1 <- dnorm(1:20*0.5, 4.25, 1.5) # find the density function with x=0.5, 1, 1.5...., 10
n2 <- pnorm(1:20*0.5, 4.25, 1.5) # find the cdf with x=0.5, 1, 1.5, ....., 10
n3 <- qnorm(1:9*0.1, 4.25, 1.5)  # find the 10, 20, .., 90 percentiles of normal
n4 <- rnorm(25, 4.25, 1.5) # generate 25 normal random variables
plot(1:20*0.5, n1, col="red", type="l") # plot the density curve from 0.5 to 10 

# Example 6: exponential Distribution
# lambda =2.5 e.g. average accidents (number of events) per day
e1 <- dexp(0.2*1:16, 2.5)
e2 <- pexp(0.2*1:16, 2.5)
e3 <- qexp(c(0.25, 0.5, 0.75, 0.9), 2.5)
e4 <- rexp(25, 2.5)
plot(0.2*1:16, e1, col="green", type="l")


#Week 2

# Confidence interval population mean
lunch <- c(7.2, 6.4, 8.2, 6.3, 7.9, 10.5, 5.5, 6.2, 7.5, 6.8, 8.5, 7.3, 
           7.0, 11.5, 8.5, 6.4)
t.test(lunch, alternative="two.sided", conf.level=0.95)
"The 95% confidence interval for the average lunch spending is (6.759,  8.453)"

# Confidence interval population proportion
CI2 <- prop.test(3664, n=6543, alternative="two.sided",
                 conf.level=0.95, correct=FALSE) #0.56*6543 = 3664

# one-sample t-test for mean
t.test(lunch, alternative="two.sided", mu=6.0, 
       conf.level=0.95)
"Since the absolute value of test statistic is 4.0426 which is more than critical value 2.13145 (or p-value 0.001063 less than 0.05), we reject null hypothesis.
The data provide sufficient evidence that the mean spending is different from $6. "

# one sample Z-test for proportion
prop.test(185, n=500, p=0.3333, alternative="two.sided",
                      conf.level=0.95, correct=FALSE)

"Since the p-value is more than 0.05, we don't reject null hypothesis. The data do not provide evidence that the proportion is different from one-third"

# two sample t-test for difference of population means

dinner <-c(8.3, 6.8, 8.0, 7.5, 8.3, 10.2, 8.5, 7.4, 8.5, 7.8, 8.3, 8.3,
           9.2, 10.4, 9.5, 7.4)
t.test(lunch, dinner,  alternative="two.sided", mu=0, 
                 var.equal=TRUE, conf.level=0.95) # var.equal usually true unless specified

"Since the p-value is 0.1014 more than 0.05, we don't reject null hypothesis.
The data do not provide sufficient evidence that mean spending for lunch is different from the mean spending for dinner.
"

# two sample proportion test for male prop = female prop
prop.test(c(136, 224), n=c(240, 260),  alternative="two.sided",
                      conf.level=0.99, correct=FALSE)
"The data provide sufficient evidence to show difference between males and females 
in the proportion who enjoy shopping for clothing at the 0.01 level of significance"


# week 3

# chi-square distribution

c1 <- dchisq(1:40, 15)  # chi-square pdf with df=15
c2 <- pchisq(1:40, 15)  # chi-square cdf with df=15
c3 <- qchisq(c(0.01, 0.05, 0.1, 0.9, 0.95, 0.99), 15) # chi-square percentiles
c10 <- dchisq(1:40, 10)  # chi-square pdf with df=10
plot(1:40, c10, col="green", type="l", xlab="X values", ylab="density",
     main="Chi-square pdf with 10 and 15 df ") # main plot
lines(1:40, c1, col="red", type="l") # add line
legend(25, 0.08, lty=1, col=c("green", "red"), legend= c("df=10", "df=15")) # add legend

# poison modeling
library(fitdistrplus)  # call the package function every time you use it
death <- c(rep(0,484), rep(1,391), rep(2, 164), rep(3,45), rep(4,11), rep(5,1))  # generate the data set
fpois <- fitdist(death, distr="pois")  # fit the data set to Poisson distribution
# chi square break class 0,1,2,3 and more than 3, each class should have count at least 5
result1 <- gofstat(fpois, chisqbreaks=c(0:3), discrete=TRUE, 
                   fitnames=c("Poisson"))  # run the goodness of fit test
plot(fpois)
summary(fpois)
result1

# exponential modeling

volcano <- c(126, 73, 3, 6, 37, 23, 73, 23, 2, 65, 
             94, 51, 26, 21, 6, 68, 16, 
             20, 6, 18, 6, 41, 40, 18, 41, 11, 12, 
             38, 77, 61, 26, 3, 38, 50, 91, 12)
fexp <- fitdist(volcano, distr= "exp")
fnorm <- fitdist(volcano, distr= "norm")
summary(fexp)
summary(fnorm)
plot(fexp)
plot(fnorm)
"Both AIC and BIC of exponetial distribution "

#Log normal stock price model (SingTel data)  
Z74SI <- read.csv("Z74SI.csv")
price <- Z74SI$Adj.Close  # put the closing price to object "price"
volume <- Z74SI$Volume
rate <- NULL  # define a new object "rate" without any elements
# calculate the rates of daily change and put them to object "rate"
for (i in 1:(length(price)-1)) 
{
  rate[i] <- (price[i]-price[i+1])/price[i+1]
  } 
frate <- fitdist(rate, "norm")
summary(frate)
plot(frate)

#Historical 99% VaR of one day for SingTel investment 4000 shares at $3.30
AbsVaR <- -1*quantile(rate, 0.01)*4000*3.30
meanR <- mean(rate)*4000*3.30
SingVaR1 <- meanR+AbsVaR

#Parametric 99% VaR of one day for SingTel investment 4000 shares at $3.30
SingVaR2 <- sd(rate)*4000*3.30*qnorm(0.99, 0, 1)


# week 4

STAT101 <- read.csv("STAT101.csv")
datatest <- STAT101
test <- datatest$Test  # define Test column in data set as test
assign <- datatest$Assignments
# using lm() function to fit the line with 'test' as response and 'assign' as predictor 
# y ~ x
reg.fit <- lm(test~assign) 
summary(reg.fit)
names(reg.fit)
# plotting x first then y
plot(assign, test, main="Linear relationship between 
     assignment scores and test scores in STAT101", 
     xlab="Assignment scores", ylab="Test scores")
abline(reg.fit, lwd=3, col="red")

resid <- residuals(reg.fit)
plot(assign, residuals(reg.fit), main="Relationship between 
     assignment scores and residuals", 
     xlab="Assignment scores", ylab="Residuals")

# model assumptions checking
library(fitdistrplus)
fnorm <- fitdist(resid, "norm")
result <- gofstat(fnorm, discrete=FALSE)
result
# for Kolmogorov-Smirnov test:  
# critical value is 1.22/sqrt(n),1.36/sqrt(n),1.63/sqrt(n) for alpha=0.10,0.05,0.01
KScritvalue <-1.36/sqrt(length(test))
KScritvalue
summary(fnorm)
plot(fnorm)

# prediction and estimation
confint(reg.fit, level=0.95) # confidence interval for coefficient

predict(reg.fit, data.frame(assign=c(60, 70, 86, 100, 110)),
        interval="confidence", level=0.95) # confidence interval for mean of Yi
predict(reg.fit, data.frame(assign=c(60, 70, 86, 100, 110)),
        interval="prediction", level=0.95) # prediction interval for Yi

# week 5

# multiple regression model
adv <- read.csv("Advertising1.csv")
pairs(adv) # scatter plot pairs
lm.sale3 <- lm(sales~TV+radio+newspaper, data=adv)
AIC(lm.sale3)
BIC(lm.sale3)

# interaction model
lm.sale4 <- lm(sales~TV*radio, data=adv)
summary(lm.sale4)
attach(adv)
plot(lm.sale4$fitted.values, residuals(lm.sale4),
     main="Relationship between predicted sales and residuals",
     xlab="sales", ylab="residuals") # residual vs y
plot(TV, residuals(lm.sale4), main="Relationship between TV and 
     residuals", xlab="TV", ylab="residuals") # residual vs X1
plot(radio, residuals(lm.sale4), main="Relationship between radio and 
     residuals", xlab="radio", ylab="residuals") # residual vs X2
plot(TV*radio, residuals(lm.sale4), main="Relationship between interaction and residuals",
     xlab="TV*radio", ylab="residuals")# residual vs X1X2
library(fitdistrplus)
fnorm <- fitdist(residuals(lm.sale4), distr="norm")
summary(fnorm)
plot(fnorm)
confint(lm.sale4, level=0.95)
predict(lm.sale4, data.frame(TV=20, radio=15), interval="confidence", level=0.95)
predict(lm.sale4, data.frame(TV=20, radio=15), interval="prediction", level=0.95)

# quadratic relationship
library(ISLR)
lm.carseat5=lm(Sales~CompPrice+Income+Advertising+
                 Price+ShelveLoc+Age+I(Price^2), data=Carseats)# I() don't include based term, Poly include
summary(lm.carseat5)


# week 6

# Logistics Regression
library(ISLR)
attach(Default)
glm.def1 <- glm(default~ balance, data=Default, family=binomial)
summary(glm.def1)

# Logistics regression p value
pvalue1 <- with(glm.def1, pchisq(null.deviance- deviance, df.null-df.residual, lower.tail=FALSE))
pvalue1

# confusion matrix
glm.def5 <- glm(default~ balance+student, data=Default, family=binomial)
summary(glm.def5)
glm.prob5 <- predict(glm.def5, type="response")
glm.pred5.1 <- rep("Predicted No Default", 10000)
glm.pred5.1[glm.prob5> 0.5] <- "Predicted Default"
table(glm.pred5.1, default)

# logistics regression prediction - manual

ff <- function(a, b, x) {
  aa <- exp(a+b*x)
  ff <- aa/(1+aa)
  ff
}
r1 <-ff(-10.65, 0.0055, 1000)
r1

# Plot logistics regression - manual

bb <- c(1:100*30)
r3 <- ff(-10.65, 0.0055, bb)
plot(bb,r3, main="Estimated probability of default using logistic regression",
     xlab="Balance", ylab="Prob. of Default")

# Deviance residual
y <- c(1,1,1,0,0)
x <- c(24, 27, 29, 28, 32)

dev_stats <- function(y_true,y_pred){
  d <- NULL
  for(i in 1:length(y_true)){
    if (y_true[i] >= y_pred[i]){
      d[i] <- sqrt(2*-log(1-(1-y_pred[i])))
    }
    else{
      d[i] <- -sqrt(2*-log(1-(y_pred[i]-0)))
    }
  }
  return(d)
}

dev_stats(y[1],predict(glm.re, data.frame(x=c(24)), type="response"))
dev_stats(y,predict(glm.re, data.frame(x=c(24, 27, 29, 28, 32)), type="response"))

# Deviance residuals
ds <- dev_stats(y,predict(glm.re, data.frame(x=c(24, 27, 29, 28, 32)), type="response"))
resdev <- sum(ds^2)
resdev


# week 9

# validation set approach
library(ISLR)
attach(Auto)
RNGkind(sample.kind="Rounding") 
set.seed(141)
train <- sample(392, 196)
lm.fit1 <- lm(mpg~horsepower, data=Auto, subset=train)
mse1 <- mean((mpg-predict(lm.fit1, Auto))[-train]^2)
mse1

# LOOCV - min bias, max variance
library(boot)
RNGkind(sample.kind="Rounding") 
set.seed(141)
glm.fit1 <- glm(mpg~horsepower, data=Auto)
cv.err1 <- cv.glm(Auto, glm.fit1)
cv.err1$delta

#LOOCV with complex polynomial fits
RNGkind(sample.kind="Rounding") 
set.seed(141)
cv.err2 <- rep(0,10)
for (i in 1:10)  {
  glm.ff <- glm(mpg~poly(horsepower,i, raw=TRUE), data=Auto)
  cv.err2[i] <-cv.glm(Auto, glm.ff)$delta[1]}
cv.err2
plot(c(1:10), cv.err2, type="b", main="LOOCV", 
     xlab="Degree of Polynomial", ylab="MSE")

#  K-fold CV where K=5
RNGkind(sample.kind="Rounding") 
set.seed(141)
cv.err3 <- rep(0,10)
for (i in 1:10)  {
  glm.ff <- glm(mpg~poly(horsepower,i, raw=TRUE), data=Auto)
  cv.err3[i] <-cv.glm(Auto, glm.ff, K=5)$delta[1]}
cv.err3
plot(c(1:10), cv.err3, type="b", main="5-fold CV", 
     xlab="Degree of Polynomial", ylab="MSE")

# bootstrap to estimate 95% CI of the slope of linear regression model
RNGkind(sample.kind="Rounding") 
set.seed(141)
boot.fn <- function(da, ind)
  return(coef(lm(mpg~horsepower, data=da, subset=ind)))
boot.fn(Auto, sample(392, 392, replace=T))  # bootstrap one time only
bs_result <- boot(Auto, boot.fn, R=10000)  #use boot function to bootstrap 10000 times
c(quantile(bs_result$t[,2], 0.025), quantile(bs_result$t[,2], 0.975)) #bootstrap CI

lm.ff <- lm(mpg~horsepower, data=Auto)
confint(lm.ff) # normal CI

# week 10
library(ISLR)
Hit <- na.omit(Hitters)
library(leaps)

# Best Subset Selection
library(leaps)
regfit2.all <- regsubsets(Salary~., Hit, nvmax=19) # nvmax option can obtain all models
reg2.summary <- summary(regfit2.all) # check what measurements are given in the selection process

# plotting, finding optimal and getting coefficient

plot(reg2.summary$rss, main="RSS plot", 
     xlab="Number of variables", ylab="RSS", type="b") # plot RSS
plot(reg2.summary$adjr2, main="Adjusted r^2 plot",
     xlab="Number of variables", ylab="Adjusted r^2", type="b") # plot adjusted R^2
plot(reg2.summary$cp, main="Cp plot", 
     xlab="Number of variables", ylab="Cp", type="b") # plot Cp, Cp and AIC are proportional
plot(reg2.summary$bic, main="BIC plot", 
     xlab="Number of variables", ylab="BIC", type="b") # Plot BIC, heavier penalty for many variable
a <- which.min(reg2.summary$rss)
b <- which.max(reg2.summary$adjr2) # only adjusted R^2 max
c <- which.min(reg2.summary$cp)
d <- which.min(reg2.summary$bic)
coef(regfit2.all, a)
coef(regfit2.all, b)
coef(regfit2.all, c)
coef(regfit2.all, d)

# Backward Stepwise Selection
regfit3.all <- regsubsets(Salary~., Hit, nvmax=19, method="backward") # cannot use if n<p!
reg3.summary <- summary(regfit3.all)

# Forward Stepwise Selection
regfit4.all <- regsubsets(Salary~., Hit, nvmax=19, method="forward") # work even if n<p
reg4.summary <- summary(regfit4.all)


# week 11
RNGkind(sample.kind="Rounding")
library(leaps)
library(glmnet)
Y <- c(2.46, 2.74, 3.02, 2.94, 3.21, 3.17, 3.42, 3.67)
X1 <- c(0.51, 1.01, 1.52, 1.78, 2.02, 2.28, 3.03, 3.54)
X2 <- c(1.14, 2.4, 1.14, 3.42, 3.81, 2.67, 2.09, 1.52)
lassodata <- data.frame(Y, X1, X2)

#Ridge Regression - wont reduce to 0

x <- model.matrix(Y~., lassodata)[,-1] # need to use matrix
y <- lassodata$Y
grid <- seq(15, 0, length=16)  # set lambda values using custom grid
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)  #ridge uses alpha=0, lasso uses alpha=1
norm2 <- sqrt(res$coefficients[2]^2+res$coefficients[3]^2) # calculate the norm 2
# calculate the l2 norm ratio for each lambda
l2norm <- rep(NA, 16) 
for (i in 1:16) 
  l2norm[i] <- sqrt(sum(coef(ridge.mod)[-1,i]^2))/norm2

plot(ridge.mod$lambda, coef(ridge.mod)[2,],
     main="Ridge Reg: lambda versus coef related to X1",
     xlab="lambda", ylab="estimated coef related to X1", type="p")

plot(ridge.mod$lambda, coef(ridge.mod)[3,],
     main="Ridge Reg: lambda versus coef related to X2",
     xlab="lambda", ylab="estimated coef related to X2", type="p")

plot(l2norm, coef(ridge.mod)[2,], main="Ridge Reg: l2norm versus coef related to X1",
     xlab="l2norm Ratio", ylab="estimated coef related to X1", type="p")

plot(l2norm, coef(ridge.mod)[3,], main="Ridge Reg: l2norm versus coef related to X2",
     xlab="l2norm Ratio", ylab="estimated coef related to X2", type="p")

# norm 2
norm2 <- sqrt(res$coefficients[2]^2+res$coefficients[3]^2) # calculate the norm 2

# norm 1
norm1 <- sum(abs(res$coefficients[c(2,3)]))

#Lasso approach might reduce to 0
x <- model.matrix(Y~., lassodata)[,-1] # need to use matrix
y <- lassodata$Y
grid <- seq(0.38, 0, length=20)  # set lambda values
lasso.mod <- glmnet(x, y, alpha=1, lambda=grid) #ridge uses alpha=0, lasso uses alpha=1  

norm1 <- sum(abs(res$coefficients[c(2,3)]))

# calculate the l1 norm ratio for each lambda
l1norm <- rep(NA, 20) 
for (i in 1:20) 
  l1norm[i] <- sum(abs(coef(lasso.mod)[c(2,3),i]))/norm1

plot(lasso.mod$lambda, coef(lasso.mod)[2,], main="Lasso: lambda versus coef related to X1",
     xlab="lambda", ylab="estimated coef related to X1", type="p")

plot(lasso.mod$lambda, coef(lasso.mod)[3,], main="Lasso: lambda versus coef related to X2",
     xlab="lambda", ylab="estimated coef related to X2", type="p")

plot(l1norm, coef(lasso.mod)[2,], main="Lasso: l1norm versus coef related to X1",
     xlab="l1norm", ylab="estimated coef related to X1", type="p")

plot(l1norm, coef(lasso.mod)[3,], main="Lasso: l1norm versus coef related to X2",
     xlab="l1norm", ylab="estimated coef related to X2", type="p")


# Ridge with cv
library(leaps)
library(glmnet)
x <- model.matrix(Salary~., Hit)[,-1]
y <- Hit$Salary
grid <- 10^seq(10, -2, length=100)
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2) # get half of data as training set
test <- (-train)  # the rest as testing set
y.test <- y[test]
y.train <- y[train]
ridge.mod <- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)

set.seed(1)
cvrr.out <- cv.glmnet(x[train,], y[train], alpha=0, nfolds = 10) #obtain the 10-fold cross validation error
plot(cvrr.out)
bestlam <- cvrr.out$lambda.min  #identify the lambda for smallest CV error
bestlam
ridge.pred <-predict(ridge.mod, s=bestlam, newx=x[test,])  
mean((ridge.pred-y.test)^2)  # obtain the MSE from the test set
out.rr <- glmnet(x,y,alpha=0, lambda=grid) # different from textbook
# get the final model with y-intercept and 19 coefficients by using the best lambda
predict(out.rr, type="coefficients", s=bestlam)[1:20] 


#lasso with cv
grid <- 10^seq(10, -2, length=100)
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1,nfolds = 10)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)
out.lasso <- glmnet(x,y, alpha=1, lambda=grid)
lasso.coef <- predict(out.lasso, type="coefficients", s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]


# week 12

# Regression tree
library(tree)
library(MASS)
attach(Boston)
set.seed(1)
train1 <- sample(1:nrow(Boston), nrow(Boston)/2)
test1 <- -train1
tree.boston <- tree(medv~., Boston, subset=train1)
summary(tree.boston)
tree.boston

#plotting tree
plot(tree.boston)
title ("Regression Tree for Boston data")
text(tree.boston, pretty=0) # pretty=0 include the category names for any qualitative predictors

#pruning tree using cross validation
cv.boston <- cv.tree(tree.boston)  # use the cv.tree function to prune the tree.boston
cv.boston
plot(cv.boston$size, cv.boston$dev, type="b", main="Cross validation: Deviance versus Size",
     xlab="Number of terminal nodes", ylab="deviance")
nn <- cv.boston$size[which.min(cv.boston$dev)]  # identify the optimal number of nodes 
nn
prune.boston <- prune.tree(tree.boston, best=nn) # prune the tree with optimal size
# plotting tree
plot(prune.boston)
title ("Pruned Regression Tree for Boston data")
text(prune.boston, pretty=0)

# prediction error of optimal pruned tree on test set
yhat1 <- predict(prune.boston, newdata=Boston[test1,])
boston.test1 <- Boston[test1, "medv"]
plot(yhat1, boston.test1, main="Pruned Tree prediction versus observed sales for test data",
     xlab="predict sales", ylab="observed sales")
abline(0,1)
mean((yhat1-boston.test1)^2) # mean squared error

# Final regression tree model 
tree.bostonall <- tree(medv~., Boston)
prune.bostonall <- prune.tree(tree.bostonall, best=nn) 
plot(prune.bostonall)
title ("Pruned Regression Tree for all Boston data")
text(prune.bostonall, pretty=0)

# Classification Trees
library(ISLR)
attach(Carseats)
str(Carseats)
HighSale <- ifelse(Sales <= 8, "No", "Yes")
CarNew <- data.frame(HighSale, Carseats[,-1], stringsAsFactors = TRUE)  
tree.car <- tree(HighSale~., CarNew)
summary(tree.car)
# Plotting tree
plot(tree.car)
title(main="Classification tree for Carseats data")
text(tree.car, pretty=0)

# cross-validation to determine the optimal level of tree complexity
set.seed(2)
train <- sample(1:nrow(CarNew), 200)  # use 200 data as training data
CarNew.test <- CarNew[-train,]        # set the other 200 as test data
high.test <- HighSale[-train]
tree.car1 <- tree(HighSale~., CarNew, subset=train)
tree.pred <- predict(tree.car1, CarNew.test, type="class") #for class prediction

table1 <- table(tree.pred, high.test)
sensitivity1 <- table1[2,2]/sum(table1[,2]) # yes-yes / all true yes
sensitivity1
totalerror1 <- (table1[1,2]+table1[2,1])/sum(table1)  # total error rate
totalerror1
table1

# consider whether pruning the tree can improve the results
set.seed(3)
cv.car <-cv.tree(tree.car1, FUN=prune.misclass)  #indicate the classification error rate
plot(cv.car$size, cv.car$dev, type="b",
     main="Pruning classification tree: size versus deviance",
     xlab="number of nodes", ylab="error")
nn <- cv.car$size[which.min(cv.car$dev)]
prune.car1 <- prune.misclass(tree.car1, best=nn)
plot(prune.car1)
title(main="pruned classification tree with optimal size")
text(prune.car1, pretty=0)
tree.pred <- predict(prune.car1, CarNew.test, type="class")

table2 <- table(tree.pred, high.test)
sensitivity2 <- table2[2,2]/sum(table2[,2])
sensitivity2
totalerror2 <- (table2[1,2]+table2[2,1])/sum(table2)
totalerror2
table2

# Final classification tree model 
tree.car <- tree(HighSale~., CarNew)
prune.car1 <- prune.misclass(tree.car, best=2)
plot(prune.car1)
title ("Pruned Tree for all Car data")
text(prune.car1, pretty=0)























