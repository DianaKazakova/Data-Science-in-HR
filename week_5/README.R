#In this section how to apply control is learned (how to identify the 
#coefficient of independent part of the variable). Also I learned how to build
#the LTE model in R and how to interpret it. Also I learned how to estimate LTE 
#model with the help of sample splitting techniques. 


#Controls 
oj <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_2//oj.csv")

basefit <- lm(log(sales) ~ log(price), data=oj)
coef(basefit) #cannot interpret it as a causal effect because there are many variables which are omitted

#to control brand effect we added brand variable in the regression
#here we can see that 2 other brand have a positive effect on sales 
#compared to dominics brand.
brandfit <- lm(log(sales) ~ brand + log(price), data=oj)
coef(brandfit)

#Part of price can be explained by brand, we are trying to find the independent part which cannot be explained by brand
pricereg <- lm(log(price) ~ brand, data=oj) #regresses log(price) onto brand
phat <- predict(pricereg, newdata=oj) # use the model to predict log price
presid <- log(oj$price) - phat #the residuals from regression of log price on brand
residfit <- lm(log(sales) ~ presid, data=oj) # use residuals in fitting the model of log sales. The coefficient is equal to the one from brandfit
coef(residfit) #finding the coefficients on the part of each input that is independent from the other inputs

#LTE model of the abortion example
data <- read.table("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_5//abortion.dat", skip=1, sep="\t")

#data preporation
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
                 "a_murd","a_viol","a_prop",'prison','police',
                 'ur','inc','pov','afdc','gun','beer')
data <- data[!(data$state%in%c(2,9,12)),] # AK, DC, HA are strange places
data <- data[data$year>84 & data$year<98,] # incomplete data outside these years
data$pop <- log(data$pop)
t <- data$year - 85
s <- factor(data$state) ## states are numbered alphabetically
controls <- data.frame(data[,c(3,10:17)])
y <- data$y_murd
d <- data$a_murd

#building the model
orig <- glm(y ~ d + t + s + ., data=controls) # run linear regression with all variables
summary(orig)#all coefficients

summary(orig)$coef['d',] #only variable d

dcoef <- summary(orig)$coef['d',][1] #extract only beta coefficient

#to interpret it we need to take an exponent of it. As the beta is negative we need to use formula 1-(exp(b)) 
1-exp(dcoef) # One more abortion per ten live births decreases the per capita murder rate by 19%



cell <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_5//us_cellphone.csv")

cellrate <- 5*cell[,2]/(1000*cell[,3]) # center on 1985 and scale by 1997-1985

library(gamlr)
## refactor state to have NA reference level
sna <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x <- sparse.model.matrix( ~ t + phone*sna + .^2, data=controls)[,-1]

dim(x)
## naive lasso regression
naive <- cv.gamlr(cbind(d,x),y) # run the regression with all variables but our data set is high dementional 

head(coef(naive))
coef(naive)["d",]  # receive the negative coefficient of abbortion rate

# follow the Algorithm of LTE Lasso Regression

# run th regression to calculate d based on x. The parameter lmr=1e-3 sets the 
#threshold for the lasso regularization. It controls the amount of shrinkage applied 
#to the coefficients of the model. A smaller value of lmr leads to stronger regularization.

treat <- cv.gamlr(x,d, lmr=1e-3) 
head(summary(treat))

#use the model to predict new data of x based on the lambda min
predtreat <- predict(treat, x, select="min")
head(predtreat)

# drop function is used when we need to delete the dimensions of an array which have only one level.
dhat <- drop(predtreat) 
dhat
length(dhat)


par(mai=c(.9,.9,.1,.1))
plot(dhat,d,pch=21,bty="n",bg=8, cex=.8,yaxt="n")
axis(2, at=c(0,1,2,3)) 

#The in-sample R2 is around 99%. It means that there is almost no independent 
#movement of abortion rates to measure as effecting crime.
cor(drop(dhat),d)^2

# re-run lasso, with this (2nd column) included unpenalized (free=2)
causal <- cv.gamlr(cbind(d,dhat,x),y, free=2, lmr=1e-3) # free parameter leaves dhat column unpenalized
coef(causal, select="min")["d",] # LTE lasso regression finds no evidence for effect of abortion on murder.

#To estimate lasso regression we can use sample splitting procedure.
library(gamlr)
data(hockey)
head(goal, n=2)

player[1:2, 2:7] #players on ice. +1 is home players. 0 is off ice.
team[1, 2:6] #Sparse Matrix with indicators for each team*season interaction: +1 for home team, -1 for away team. 
config[5:6, 2:7] #Special teams info. For example, S5v4 is a 5 on 4 powerplay, +1 if it is for the home-team and -1 for the away team.

#sample splitting example
x <- cbind(config,team,player) # create a subset of the data
y <- goal$homegoal # create a y for the model

fold <- sample.int(2,nrow(x),replace=TRUE) #divide the data into 2 parts

nhlprereg <- gamlr(x[fold==1,], 
                   y[fold==1], 
                   family="binomial", 
                   standardize=FALSE) #fit a regression model to half of the data

#  select which columns of x have a nonzero coefficient. We used -1 to delete the intercept
selected <- which(coef(nhlprereg)[-1,] != 0)

xnotzero <- as.data.frame(as.matrix(x[,selected]))  

nhlmle <- glm( y ~ ., data=xnotzero, 
               subset=which(fold==2), family='binomial' )

#we can observe standard errors via summary function
summary(nhlmle)

#Let's consider the example where the objective is to predict the probability 
#that a goal was from the home team

x[1,x[1,]!=0] #check first observation for players on the ice

#calculating regresion fit
prediction<-predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)

CI <- prediction$fit + c(-2,2)*prediction$se.fit
CI #90% confidence interval for probability that Edmonton scored the goal is 28% to 57%

#Controls part 2
install.packages('AER')
library(AER)
library(gamlr)

# Orthogonal ML R Function
orthoLTE <- function(x, d, y, dreg, yreg, nfold=2)
{
  # randomly split data into folds
  nobs <- nrow(x)
  foldid <- rep.int(1:nfold, 
                    times = ceiling(nobs/nfold))[sample.int(nobs)]
  I <- split(1:nobs, foldid)
  # create residualized objects to fill
  dtil <- rep(NA, nobs)
  ytil <- dtil
  # run OOS orthogonalizations
  cat("fold: ")
  for(b in 1:length(I)){
    dfit <- dreg(x[-I[[b]],], d[-I[[b]]])
    yfit <- yreg(x[-I[[b]],], y[-I[[b]]])
    dhat <- predict(dfit, x[I[[b]],], type="response")
    yhat <- predict(yfit, x[I[[b]],], type="response")
    dtil[I[[b]]] <- drop(d[I[[b]]] - dhat)
    ytil[I[[b]]] <- drop(y[I[[b]]] - yhat)
    cat(b," ")
  }
  rfit <- lm(ytil ~ dtil)
  gam <- coef(rfit)[2]
  se <- sqrt(vcovHC(rfit)[2,2])
  cat(sprintf("\ngamma (se) = %g (%g)\n", gam, se))
  
  return( list(gam=gam, se=se, dtil=dtil, ytil=ytil) )
}

dreg <- function(x,d){ cv.gamlr(x, d, lmr=1e-5) } # prepare the function of CV lasso
yreg <- function(x,y){ cv.gamlr(x, y, lmr=1e-5) } # prepare the function of CV lasso

# OrthoML and effect of abortion access on crime
resids <- orthoLTE( x=x, d=d, y=y, 
                    dreg=dreg, yreg=yreg, nfold=5) 
head(resids$dtil)
head(resids$ytil)

#the result is a small and statistically insignificant positive treatment effect 
#estimate (p-value ≈ 0.7). p-value supports no effect of abortion access on crime
2*pnorm(-abs(resids$gam)/resids$se) 


