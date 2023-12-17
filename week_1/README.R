#Introduction

# In this section the basics of data analysis in R are learned (how to obtain the 
#main information about the data set, dimension, how to index the data set etc)
#Also how to build the basic graphs in R and build a simple regression (+ analysis of coefficients)

CEO_Diary<-read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_1//survey_response_data.csv") # read the data 
dim(CEO_Diary) # analyse the dimentions of the table

View(CEO_Diary) # view the table in a separate window

CEO_Diary[1:25,c(1:3,37,38, 39, 40)] # filtering the columns

apply(CEO_Diary,2,class) # use class function for every columns in the data 
apply(CEO_Diary,2,typeof) # use type of function for every columns in the data

lapply(CEO_Diary,class) # use class for every columns and make a list
sapply(CEO_Diary,class) # use class function and create an array 

nrow(CEO_Diary) # number of rows
ncol(CEO_Diary) # number of columns

summary(CEO_Diary[1:5]) # provide a summary of first 5 columns, show the main information about this variables (type, mean, max, median etc)

summary(CEO_Diary) # provide a summary for every columns

par(mar=c(9, 3 ,1,1)) # change the default parameters
barplot(prop.table(table(CEO_Diary$type)), las=2)
dev.off() # use to clean the plot section

par(mar=c(12, 5 ,2,2)) # change the parameters to analyse the differences
barplot(prop.table(table(CEO_Diary$F_duration)), las=2) # make a barplot for duration


table(CEO_Diary$type); # create a table with frequency
prop.table(table(CEO_Diary$type)); # create the proportion of the frequency
# create a barplot of the proportion of the frequency to analyse what is the most popular type of activity of CEO
barplot(prop.table(table(CEO_Diary$type))) 

#build the regression where strategy is dependent variable and consultants and politicians 
#are independent variables (analyse the relationship between dependent and independent variables)
fit <- glm(strategy ~ consultants + politicians, data=CEO_Diary); 

# from the estimated coefficient we can see that consultants variable has a positive 
#influence (0.0113590) of the strategy variable and politicians variable has a negative 
#influence (-0.0241179) on the strategy variable. Every coefficient are significant 
#at the approximately 0% significance level. 
summary(fit) 

# with this function we can obtain only the table with coefficients
summary(fit)$coef

summary_fit<-summary(fit) # assign new variable
summary_fit$deviance # select data from summary statistics


#Uncertainty

#In this section the frequency uncertainty and bootstrap sampling are learned. 
#The importance of replacement parameter is highlighted because of the necessary 
#to estimate the variance of the samples. Also, the CLT are learned regarding the 
#bootstrap and frequency uncertainty algorithms. Besides, the possibility to use 
#bootstrap to evaluate the uncertainty in regression coefficients are also learned 

browser<-read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_1//web-browsers.csv") # read the data 
dim(browser) # analyse the dimensions of the data set

head(browser) # analyse the 6 first rows of the data set to see how the data looks like

mean(browser$spend) # ana.yse the mean of the spens variable with its sample 

#plot the histogramm of logged spend. The distribution is quate normal (gausian distribution)
histogram(log(browser$spend))

var(browser$spend)/1e4 # estimation the variance of the spend variable

sqrt(var(browser$spend)/1e4) # calculate a standard deviation

#boostraping
B<- 1000
mub <- c()
for (b in 1:B){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)
hist(mub) # plot the histogram of bootstrap estimation. We can see the normal distribution of the mean X from the samples

#we can present the overlaping of frequency and bootstrap results
par(mar=c(5, 4 ,3,2)) 
h <- hist(mub) # plot the histogram of bootstrap estimation
xfit <- seq(min(mub), max(mub), length = 40)

#calculate the probability density where the mean is the true mean of the original sample and sd is the sd of the original sample
yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4))

# scale the normal distribution in yfit to fit the histogram. diff(h$mids[1:2]) is 
#the weight of the bar, length(mub) os the number of bars
yfit <- yfit * diff(h$mids[1:2]) * length(mub)
lines(xfit, yfit, col = "black", lwd = 2)

#what is the importance of the number of bootstrap samples? Let's try some examples 
#the first example with 500 samples
set.seed(1)
A <- 50
mub_1 <- c()
for (b in 1:A){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub_1 <- c(mub_1, mean(browser$spend[samp_b]))
}
sd(mub_1)

#the second example with 1000 samples
B <- 1000
mub_2 <- c()
for (b in 1:B){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub_2 <- c(mub_2, mean(browser$spend[samp_b]))
}
sd(mub_2)


#the third example with 1500 samples
C <- 15000
mub_3 <- c()
for (b in 1:C){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub_3 <- c(mub_3, mean(browser$spend[samp_b]))
}
sd(mub_3)

# Explanation: The small number of bootstrap samples can lead to inaccurate results. 
#At the same time, big number of samples requires a lot of time to run the
#bootstrap but can provide more stable results. 

# Analyse the importance of replacement parameter
# trying not to replace
C <- 1500
mub_3 <- c()
for (b in 1:C){
  samp_b <- sample.int(nrow(browser), replace=FALSE)
  mub_3 <- c(mub_3, mean(browser$spend[samp_b]))
}
sd(mub_3)

#we have 0 sd because sampling without replacement do not generate variability across 
#samples - this is a basic idea of the bootstrap.


# to evaluate how close bootstrap to the truth we can use bootstrap too compare with regression coefficients
#But here the dimension matters (do not use more than 3 variables in bootstrapping)
reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser)
summary(reg_b) # true linear regression 

#bootstrapped coefficients for broadband and anychildren
betas <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,])
  betas <- rbind(betas, coef(reg_b)) 
}
head(betas, n=5)
nrow(betas) # equal to the sample size

mean(betas[,"broadband"])
mean(betas[,"anychildren"])

#estimating covariances between estimators
cov(betas[,"broadband"], betas[,"anychildren"])


