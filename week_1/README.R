#Introduction

CEO_Diary<-read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_1//survey_response_data.csv")
dim(CEO_Diary)

View(CEO_Diary)

CEO_Diary[1:25,c(1:3,37,38, 39, 40)]

apply(CEO_Diary,2,class)
apply(CEO_Diary,2,typeof)

lapply(CEO_Diary,class)
sapply(CEO_Diary,class)

nrow(CEO_Diary)
ncol(CEO_Diary)

summary(CEO_Diary[1:5])

summary(CEO_Diary)

par(mar=c(9, 3 ,1,1)) # change the default parameters
barplot(prop.table(table(CEO_Diary$type)), las=2)
dev.off()

barplot(prop.table(table(CEO_Diary$type))); # create a barplot of the proportion of the frequency
table(CEO_Diary$type); # create a table with frequency
prop.table(table(CEO_Diary$type)); # create the proportion of the frequency

fit <- glm(strategy ~ consultants + politicians, data=CEO_Diary); 
summary(fit)
summary(fit)$coef

summary_fit<-summary(fit) # assign new variable
summary_fit$deviance #trying to select data from summary statistics


#Uncertainty

browser<-read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_1//web-browsers.csv")
dim(browser)

head(browser)

#frequentist
mean(browser$spend); 
var(browser$spend)/1e4; 
sqrt(var(browser$spend)/1e4)

#boostraping
B<- 1000
mub <- c()
for (b in 1:B){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub <- c(mub, mean(browser$spend[samp_b]))
}
sd(mub)

#overlaping frequency and boostrap results
h <- hist(mub)
xfit <- seq(min(mub), max(mub), length = 40)
yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4))
yfit <- yfit * diff(h$mids[1:2]) * length(mub)
lines(xfit, yfit, col = "black", lwd = 2)

#what is the importance of the number of bootstrap samples? Let's try some examples 
#the first example with 500 samples
set.seed(1)
A <- 500
mub_1 <- c()
for (b in 1:A){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub_1 <- c(mub_1, mean(browser$spend[samp_b]))
}
sd(mub_1)

#what is the importance of the number of bootstrap samples? Let's try some examples 
#the second example with 1000 samples
B <- 1000
mub_2 <- c()
for (b in 1:B){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub_2 <- c(mub_2, mean(browser$spend[samp_b]))
}
sd(mub_2)


#the third example with 1500 samples
C <- 1500
mub_3 <- c()
for (b in 1:C){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub_3 <- c(mub_3, mean(browser$spend[samp_b]))
}
sd(mub_3)

# Explanation: We have 79.66165 for 500 samples, 80.48035 for 1000 samples, and 81.96043 for 1500 samples. 
#Increase in the total number of samples leads to the increase in standard deviation 
#because the variance of samples data increases

# trying not to replace
C <- 1500
mub_3 <- c()
for (b in 1:C){
  samp_b <- sample.int(nrow(browser), replace=FALSE)
  mub_3 <- c(mub_3, mean(browser$spend[samp_b]))
}
sd(mub_3)

#we have 0 sd. sampling without replacement would generate no variability across 
#samples - this is a basic idea of the boostrap.

#Bootstrapping regressions
betas <- c()
for (b in 1:1000){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,])
  betas <- rbind(betas, coef(reg_b))
}; 
head(betas, n=5)
nrow(betas) # equal to the sampe size

#estimating covariances between estimators
cov(betas[,"broadband"], betas[,"anychildren"])


