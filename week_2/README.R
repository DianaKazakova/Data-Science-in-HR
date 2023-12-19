# Uncertainty lecture

#In this section FDR is learned. The BH Algorithm is applied in R and the process of its calculation is learned.
#Also, the hypothesis testing is learned (t test and p value from regression results) from the main book.

#BH Algorithm

browser<-read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_2//web-browsers.csv") #read data set

spend_regression <- glm(log(spend) ~ . -id, data=browser) # run linear regression without id variable
summary(spend_regression) # there are 4 variables, except intercept, which are significant at 0% significance level (based on the p value)

#1 step - pick the target
q<-0.1

#2 step - select only p values of our regression without intercept 
pval<-summary(spend_regression)$coefficients[-1,4] # we can use 4 index of necessary column or use the name of this column - Pr(>|t|)

#3 step - order p-values from smallest to largest
pvalrank <- rank(pval)

#number of hypothesis = 9
m<-length(pval)

#4 step computing the adjuster targets
pval_targets<-(pvalrank/m)*q

#find the largest i (assign it to k)
k<-max(pvalrank[pval<=pval_targets])

#make the flag of results
reject <- ifelse(pvalrank<= k, 2, 1)

#make the plot of BH results. We can see rejected hypothesis (under the line). 
#There are 5 p-values that are lower than cutoff line of BH.We can call these 
#variables significant. We can expect that around q = 0.1 of them are false positives.
#A procedure that defines significance in this way has an FDR of 10%.
plot(pvalrank, pval, ylab="p-value", xlab="p-value rank", pch=16, col=reject)
lines(pvalrank,(pvalrank/m)*q)
legend("topleft", legend=c("Rejected", "Not rejected"), col=c('red', 1), pch=16)

#BH Algorithm example 2

SC <-read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_2//semiconductor.csv") # read the data 

full <- glm(FAIL ~ ., data=SC, family=binomial) # make the logistic regression. parameter family = 'binomial' is used for logistic regression
pvals <- summary(full)$coef[-1,4] #remove coefficient for intercept and select only the column with p values
pvals
sort(pvals[!is.na(pvals)])
#we have 200 coefficients. Let' have a look at the frequency of the p value coefficients to see the main diapason of them
hist(pvals, xlab="p-value", main="", col="lightblue") # the majority of p-values is the smallest 

#pick the target for BH algorithm
q<-0.1

#apply BH algorithm with a function
fdr_cut <- function(pvals, q_value=q){
  pvals <- sort(pvals[!is.na(pvals)]) # order p-values from smallest to largest
    
  m <- length(pvals) #number of hypothesis
  
  i <- rank(pvals, ties.method="min") # compute i based on the adjusted targets
  
  k <- max(pvals[ pvals<= (i/m *q_value) ]) # find the maximum of i- assign it to k
  
  plot(pvals, log="xy", xlab="order", main=sprintf("FDR of %g",q_value),
       ylab="p-value", bty="n", col=c(8,2)[(pvals<=k) + 1], pch=20)
  lines(1:m, q_value*(1:m)/(m))
  
  return(k)
}

fdr_cut(pvals)


# Regression 

#In this section the application of linear and logistic regressions is studied. Also, the relevel 
#function (to change the baseline of the regression) is learned. Beside, the 
#interaction term in a regression is used. Also, deviance and likelihood are studied. 
#I learned how to calculate generalized R^2 based on the statistic of the summary(regression).
#Also, I lerarned how to do cross validation with for loop and how to interpret negative R^2.


oj <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_2//oj.csv")

head(oj, n=5) # print the 5 first rows

tail(oj, n=5) # print the 5 last rows

glm(log(sales) ~ brand + log(price), data=oj) # run the linear regression 
# There is no coefficient
#for dominicks brand because linear regression considering this value as a base line of the model.
#The most important positive variable is brandtropicana with the beta 1.5299, 
#brandminute.maid and brandtropicana have direct influences on dependent variable. 
#log(price) has a decreasing influence of the log(sales) variable. 


# when we use categorical variable in the regression it creates dummy variable 
#for every unique value of categorical observation

x <- model.matrix(~ brand + log(price), data=oj) 
head(x) # there are 2 columns with dummy variable for brand used in the model

#If we want to change baseline in a model we can use as.factor function for the 
#categorical variable and then relevel function to change the priority level.

oj$brand <- as.factor(oj$brand) # change the categorical variable into factor with 3 levels
str(oj) #check types of a data
oj$brand

x <- model.matrix(~ brand + log(price), data=oj) # create model matrix with a new type of brand variable

head(x) # check the results

# change the reference level of a brand variable, make "tropicana" level as a reference
oj$mybrand <- relevel(oj$brand, "tropicana")

x <- model.matrix(~ mybrand + log(price), data=oj); 

head(x)

#run the regression with changed base line
glm(log(sales) ~ mybrand + log(price), data=oj)

# calculate the regression with the interaction term of log(price), brand and feat pairwise.
# the most important positive variable is the interaction term between brandminute.maid and feat 
# brand has a positive direct influence and also its influence is increased because of the interction term
glm(log(sales) ~ log(price)*brand*feat, data=oj)

# regression with the interaction term of log(price) types of brand
glm(log(sales) ~ log(price)*brand, data=oj) 
#comparing the coefficients withth eprevious model, the coefficient of the interaction of 
#log(price):brandminute.maid is strongly decreased (0.05679 in this model vs 0.7829 in a previous model)
#this change can be explained by the difference on variables that are used in models. 
#In the previous model there is feat variable that can affect the influence of types of brand


# Logistic regression

email <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_2//spam.csv") # read the data of spam emails

spam_emails<-glm(spam ~ ., data=email, family='binomial') # family parameter lets us run logistic regression

summary (spam_emails)

#analyse particular coefficients

summary (spam_emails)$coefficients["word_3d",1] # select the coefficient of word 3d

#to analyse how the log odds of spam wil change we need to take an exponent of the coefficient 
exp(summary (spam_emails)$coefficients["word_3d",1])

#The coefficient is equal to 2.293701 and it means that spam odds increases 
#by a factor of 2 if email contains the word "3d".

# another example of selecting the coefficient for an interpretation
coef(spam_emails)["word_meeting"] #log beta of word meeting

#the coefficient is negative, therefore we need to use formula 1/exp(b) 
1/exp(coef(spam_emails)["word_meeting"]) # find an exponent of log beta. 

#The coefficient is equal to 12.22408 and it means that spam odds decreases by a factor of 12
# if email contains the word "meeting".

# prediction 
# we need to use  type = "response" because we are working wwith log odds and we 
#need to receive probabilities 

prediction<- predict(spam_emails, newdata = email[c(500,2),], type= 'response')
prediction
# The 500th and 2nd emails are true spam. The predicted probabilities say they have around 99 percent chance of being spam

#Deviance and likelihood
summary(spam_emails)

summary(spammy)$deviance # deviance is the distance between model and fit when we use beta coefficient to minimize SSE

summary(spammy)$null.deviance # is a distance between model and fit when all x are equal to 0

#we can use null deviance and residual deviance to calculate R^2

R_squared <- 1 - summary(spammy)$deviance/summary(spammy)$null.deviance
R_squared # We can explain approximately 75% of the variation in spam occurrence.


# Cross validation 

SC <- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_2//semiconductor.csv")
full <- glm(FAIL ~ ., data=SC, family=binomial) #run the logistic regression

1 - full$deviance/full$null.deviance #IS R^2

#Let's try to estimate OOS R^2
# 1 step - Define functions for the Deviance and R^2 in R.

#the function for the deviance for linear or logistic regression, Gaussian for linear regression, binomial for logistic
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) ) # calculate deviance fro linear regression
  }else{
    if(is.factor(y)) y <- as.numeric(y)>1 # change y values to 0 and 1
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) ) # calculate deviance for logistic regression
  }
}

#the function for the R^2 for linear or logistic regression, Gaussian for linear regression, binomial for logistic
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 } # change y values to 0 and 1
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}

#2 step - prepare K fold
n <- nrow(SC) # the number of observations
K <- 10 # the number of `folds' fro the cross validation is 10

# create a vector of 10 folds repeated n/K times ~148, and then shuffle them with sample(1:n) 
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]

Out <- data.frame(full=rep(NA,K)) # create an empty dataframe of results

# use a for loop to run the experiment
for(k in 1:K){
  train <- which(foldid!=k) # train on all but fold `k'
  ## fit regression on train sample
  rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
  ## get prediction on the set K
  predfull <- predict(rfull, newdata=SC[-train,], type="response")
  ## calculate and log R2
  Out$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
  ## print progress
  cat(k, " ")
}


#3 step - plot observed R^2
boxplot(Out, col="plum", ylab="R2")


colMeans(Out) # the average R2 of Out of Sample data is negative because the residual deviance of the model on the untrained data is larger
#than the null deviance (on the same data). The model fit performs worse than the intercept only model.


