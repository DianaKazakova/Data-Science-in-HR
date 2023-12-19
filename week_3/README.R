#Regularization paths
install.packages('gamlr')
library('gamlr')

#In this section I learned how to run forward stepwise regression, the results of 
#step function also helped to understand the rule of model selection (AIC)
#Also, I learned how to apply lasso regression in R and visually present the 
#regularization path of lasso. Also, different iptions of selecting the best model
#are learned (AIC, BIC, model complexity with minimum lambda and 1SE lambda)

SC <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_2//semiconductor.csv")
full <- glm(FAIL ~ ., data=SC, family=binomial) # run logistic regression on the whole data set
null <- glm(FAIL~1, data=SC) # run linear regression without x

fwd <- step(null, scope=formula(full), dir="forward") #forward stepwise regression

length(coef(fwd)) #the length of the output is 69. The model stops because lower AIC for 68-signal 
#model than for 69-signal model. The model assumes that other variable will not lead to the smallest AIC

## run a lasso path plot

web <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_3//browser-domains.csv") #read dta set

sitenames <- scan("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_3//browser-sites.txt", what="character")
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)
web$id <- factor(web$id, levels=1:length(unique(web$id)))

machinetotals <- as.vector(tapply(web$visits,web$id,sum)) 
visitpercent <- 100*web$visits/machinetotals[web$id]


xweb <- sparseMatrix(
  i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent,
  dims=c(nlevels(web$id),nlevels(web$site)),
  dimnames=list(id=levels(web$id), site=levels(web$site)))

yspend <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_3//browser-totalspend.csv", row.names=1)
yspend <- as.matrix(yspend)

spender <- gamlr(xweb, log(yspend), verb=TRUE)

#change the parameters of a function
spender <- gamlr(xweb, log(yspend)) # there is no progress printout - verb=FALSE
spender <- gamlr(xweb, log(yspend), verb=TRUE, nlambda = 50) # number of lambdas to consider (nlambda=50)

plot(spender) ## we see that we have very complex data set with more than 870 variables. 
#The lasso penalization help us to simplify it and finally we have 226 variables

# K-fold Cross Validation for LASSO

cv.spender <- cv.gamlr(xweb, log(yspend))
plot(cv.spender)
# the plot presents the out of sample error for lambdas. Blue dots related to the 
#minimum avarage OOS error. Grey lines related to the the biggest λt with average 
#OOS deviance no more than one SE. Vertical line in the middle correspond to the 
#minimum lambda, the right vertical line corresponds to the 1SE lambda. based on this 
#line we can see that the number of variables that are included in a model when we
#use minimum lambda - 226. and when we use 1SE lambda - 100

betas<-coef(cv.spender, select="min") # we should use minimum lambda if we focus on predictive performance
head(betas,10)

#another option to select the model is information criteria. The most popular - AIC 
#(it measures the distance between truth and fitted model -> need to be as small as possible).
(AIC(spender))
