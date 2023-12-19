#BH Algorithm

browser<-read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_2//web-browsers.csv")
dim(browser)

spendy <- glm(log(spend) ~ . -id, data=browser)
round(summary(spendy)$coef,2)

pval <- summary(spendy)$coef[-1, "Pr(>|t|)"]
pval

pvalrank <- rank(pval)
pvalrank

pval_targets<-(0.1/9)*pvalrank
reject <- ifelse(pval<= pval_targets, 2, 1)
k<-max(pvalrank[reject==2])


plot(pvalrank, pval, ylab="p-value", xlab="p-value rank", pch=16, col=reject)
lines(pvalrank, (0.1/9)*pvalrank)

#BH Algorithm 2

SC <-read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_2//semiconductor.csv")
dim(SC)

full <- glm(FAIL ~ ., data=SC, family=binomial)
pvals <- summary(full)$coef[-1,4] 
pvals

hist(pvals, xlab="p-value", main="", col="lightblue")

fdr_cut <- function(pvals, q=0.1){
  pvals <- sort(pvals[!is.na(pvals)])
  N <- length(pvals)
  k <- rank(pvals, ties.method="min")
  alpha <- max(pvals[ pvals<= (q*k/(N+1)) ])
  
  plot(pvals, log="xy", xlab="order", main=sprintf("FDR of %g",q),
       ylab="p-value", bty="n", col=c(8,2)[(pvals<=alpha) + 1], pch=20)
  lines(1:N, q*(1:N)/(N+1))
  
  return(alpha)
}

fdr_cut(pvals)


# Regression 

oj <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_2//oj.csv")

head(oj, n=5)

tail(oj, n=5)

glm(log(sales) ~ brand + log(price), data=oj) 
# the most important positive variable is brandtropicana with the beta 1.5299, brandminute.maid 
#and brandtropicana have direct influences on dependent variable

x <- model.matrix(~ brand + log(price), data=oj) 

head(x)

tail(x)

oj$brand <- as.factor(oj$brand)

x <- model.matrix(~ brand + log(price), data=oj); 

head(x)

oj$mybrand = relevel(oj$brand, "tropicana") # change the base group fro tropicana

x <- model.matrix(~ mybrand + log(price), data=oj); 

head(x)

# calculate the regression with the interaction term
glm(log(sales) ~ log(price)*brand*feat, data=oj)
# the most important positive variable is the interaction term between brandminute.maid and feat 
# brand has a positive direct influence and also its influence is increased because of the interction term


# Logistic regression

email <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_2//spam.csv")
dim(email)
nrow(email) # one more function
ncol(email) # one more function

colnames(email)
names(email) # one more function

spammy<-glm(spam ~ ., data=email, family='binomial') # family parameter lets us run logistic regression

coef(spammy)["word_3d"] #log beta of word 3d

exp(coef(spammy)["word_3d"]) # find an exponent of log beta. 
#The coefficient is equal to 2.293701 and it means that spam odds increases 
#by a factor of 2 if email contains the word "3d".


coef(spammy)["word_meeting"] #log beta of word meeting

1/exp(coef(spammy)["word_meeting"]) # find an exponent of log beta. 
#The coefficient is equal to 12.22408 and it means that spam odds decreases by a factor of 12
# if email contains the word "meeting".

# prediction 
prediction<- predict(spammy, newdata = email, type="response")
prediction[c(500,2500)] 
# The 500th email is true spam. The predicted probability says it has an 99 percent chance of being spam. The
#second email is real. It has just above 0 percent chance of being spam.

#deviance 
summary(spammy)

summary(spammy)$deviance

summary(spammy)$null.deviance

R_squared <- 1 - summary(spammy)$deviance/summary(spammy)$null.deviance
R_squared # 74% generalized R squared. We can explain approximately 75% of the variation in spam occurrence.


# Cross validation 

SC <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_2//semiconductor.csv")
full <- glm(FAIL ~ ., data=SC, family=binomial)

1 - full$deviance/full$null.deviance

# The first step - Define functions for the Deviance and in R.

#the function for the deviance
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) )
  }else{
    if(is.factor(y)) y <- as.numeric(y)>1
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}

#the function for the R squared
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}

# setup the experiment

n <- nrow(SC) # the number of observations
K <- 10 # the number of `folds'

foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)] # create a vector of fold memberships (random order)

Out <- data.frame(full=rep(NA,K)) # create an empty dataframe of results

# use a for loop to run the experiment
for(k in 1:K){
  train <- which(foldid!=k) # train on all but fold `k'
  ## fit regression on full sample
  rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
  ## get prediction
  predfull <- predict(rfull, newdata=SC[-train,], type="response")
  ## calculate and log R2
  Out$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
  ## print progress
  cat(k, " ")
}


boxplot(Out, col="plum", ylab="R2")


colMeans(Out) # the average R2 of Out data is negative because the deviance of the model on the untrained data is larger
#than the intercept only deviance (on the same data). The model fit performs worse than the intercept only model.


