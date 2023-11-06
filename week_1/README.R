browser<-read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_1//web-browsers.csv")
dim(browser)

head(browser)

mean(browser$spend); 
var(browser$spend)/1e4; 
sqrt(var(browser$spend)/1e4)

#what is the importance of the number of bootstrap samples? Let's try some examples 
#the first example with 1000 samples
set.seed(1)
A <- 500
mub_1 <- c()
for (b in 1:A){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub_1 <- c(mub_1, mean(browser$spend[samp_b]))
}
sd(mub_1)

#what is the importance of the number of bootstrap samples? Let's try some examples 
#the second example with 1500 samples
B <- 1000
mub_2 <- c()
for (b in 1:B){
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  mub_2 <- c(mub_2, mean(browser$spend[samp_b]))
}
sd(mub_2)


#the third example with 500 samples
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




