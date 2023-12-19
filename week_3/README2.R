#Regularization paths

SC <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_2//semiconductor.csv")
full <- glm(FAIL ~ ., data=SC, family=binomial)
null <- glm(FAIL~1, data=SC)

fwd <- step(null, scope=formula(full), dir="forward")

length(coef(fwd)) #the length of the output is 69. The model stops because lower AIC for 68-signal 
#model than for 69-signal model. This is the "best model" according to this greedy algorithm.

## run a lasso path plot

web <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_3//browser-domains.csv")
sitenames <- scan("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_3//browser-sites.txt", what="character")
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)
web$id <- factor(web$id, levels=1:length(unique(web$id)))

machinetotals <- as.vector(tapply(web$visits,web$id,sum)) 
visitpercent <- 100*web$visits/machinetotals[web$id]


xweb <- sparseMatrix(
  i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent,
  dims=c(nlevels(web$id),nlevels(web$site)),
  dimnames=list(id=levels(web$id), site=levels(web$site)))

head(xweb[2, xweb[2,]!=0]) # the sited that houshold 2 visited

yspend <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_3//browser-totalspend.csv", row.names=1)
yspend <- as.matrix(yspend)

spender <- gamlr(xweb, log(yspend), verb=TRUE)
spender

plot(spender) ## we see that we have very complex dta set with more than 870 variables. 
#The lasso penalization help us to simplify it and finally we have 226 variables

# K-fold Cross Validation for LASSO

cv.spender <- cv.gamlr(xweb, log(yspend))
plot(cv.spender)

betamin = coef(cv.spender, select="min") 
head(betamin,10)

head(AIC(spender))
