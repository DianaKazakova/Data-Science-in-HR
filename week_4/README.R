# Classification
library(MASS)
data(fgl)
dim(fgl)

#first look at the data
head(fgl, n = 2)

par(mfrow=c(2,3))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6), las=2)

#if we scale the whole data set then we have an error because the 10th column does not include numbers.
x <- scale(fgl[,1:10])

#we scale explanatory variable before measuring distances on thq 1:9 columns
x <- scale(fgl[,1:9])

#we use sd functions for every column (2 number in the middle)
apply(x,2,sd)

#we can us sd functions for every row with 1 number in the middle
apply(x,1,sd)

library(class) # load to have a knn function

#create a random sample with 10 rows 
test <- sample(1:nrow(fgl),10) #draw a random sample of 10 rows

#create knn with 1 number of neighbours considered
?knn
nearest1 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=2)

#create knn with 5 number of neighbours considered
nearest5 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=5)

data.frame(fgl$type[test],nearest1,nearest5)

Acuuracy1<- mean(fgl$type[test]==nearest1)
Acuuracy5<- mean(fgl$type[test]==nearest5)

data.frame(Acuuracy1,Acuuracy5)
#the accuracy for the nearest 5 neighbours is lower that for the 1 neighbour 
#because the model capture more data that can influence the prediction of the true label

#lad the credit data set
credit<- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_4//credit.csv")

## re-level the credit history and checking account status
credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")

credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")

credit <- credit[,c("Default", "duration", "amount",
                    "installment", "age", "history",
                    "purpose", "foreign", "rent")]

#first look at the data
head(credit)
dim(credit)

library(gamlr)
source("naref.R")
credx <- sparse.model.matrix(Default ~ . ^ 2, data=credit); 
colnames(credx)

default<- credit$Default
credscore<- cv.gamlr(credx,default,family='binomial')

par(mfrow=c(1,2))
plot(credscore$gamlr)
plot(credscore)

sum(coef(credscore, s="min")!=0)

sum(coef(credscore$gamlr, s=which.min(AIC(credscore$gamlr)))!=0)

# the OOS R^2
1 - credscore$cvm[credscore$seg.min]/credscore$cvm[1]

## What are the underlying default probabilities
## In sample probability estimates
pred <- predict(credscore$gamlr, credx, type="response")
pred <- drop(pred) # remove the sparse Matrix formatting
boxplot(pred ~ default, xlab="default", ylab="prob of default", col=c("pink","dodgerblue"))


rule <- 1/5
sum( (pred>rule)[default==1] )/sum(pred>rule) ## false positive rate at 1/5 rule

sum( (pred<rule)[default==0] )/sum(pred<rule) ## false negative rate at 1/5 rule

rule <- 2/5
sum( (pred>rule)[default==0] )/sum(pred>rule) ## false positive rate at 2/5 rule

sum( (pred<rule)[default==1] )/sum(pred<rule) ## false negative rate at 2/5 rule

sum( (pred>rule)[default==1] )/sum(default==1) ## sensitivity at 1/5 rule
sum( (pred<rule)[default==0] )/sum(default==0) ## specificity at 1/5 rule

#construction the roc curve

test <- sample.int(1000,500)
credhalf <- gamlr(credx[-test,], default[-test], family="binomial")
predoos <- predict(credhalf, credx[test,], type="response")
defaultoos <- default[test]

library(ROCR)

predoos <- predict(credhalf, credx[test,], type="response")
predoos_2 <- prediction(predoos, defaultoos)
predoos_3 <- performance(predoos_2, "tpr", "fpr")
auc <- performance(predoos_2, measure = "auc")
auc_value <- auc@y.values[[1]]

pred <- predict(credscore$gamlr, credx, type="response")
pred_2 <- prediction(pred, default)
pred_3 <- performance(pred_2, "tpr", "fpr")
auc_2 <- performance(pred_2, measure = "auc")
auc_value_2 <- auc@y.values[[1]]

par(mai=c(.9,.9,.2,.1), mfrow=c(1,2))
plot(predoos_3, main="out-of-sample")
abline(a=0, b=1, lty=2, col="red", lwd=2)
text(x=0.5, y=0.3, labels=paste("AUC =", round(auc_value, 2)), cex=.8)
## our 1/5 rule cutoff
points(x= 1-mean((predoos<.2)[defaultoos==0]), 
       y=mean((predoos>.2)[defaultoos==1]), 
       cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((predoos<.5)[defaultoos==0]), 
       y=mean((predoos>.5)[defaultoos==1]), 
       cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")


plot(pred_3, main="in-sample")
abline(a=0, b=1, lty=2, col="red", lwd=2)
text(x=0.5, y=0.3, labels=paste("AUC =", round(auc_value_2, 2)), cex=.8)
## our 1/5 rule cutoff
points(x= 1-mean((pred<.2)[default==0]), 
       y=mean((pred>.2)[default==1]), 
       cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred<.5)[default==0]), 
       y=mean((pred>.5)[default==1]), 
       cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")
dev.off()

plot(factor(Default) ~ history, data=credit, col=c(8,2), ylab="Default") 

#Multinomial Logistic Regression

library(glmnet)
xfgl <- sparse.model.matrix(type~.*RI, data=fgl)[,-1] #Design matrix includes chemical composition variable
gtype <- fgl$type
glassfit <- cv.glmnet(xfgl, gtype, family="multinomial") #cross validation experiments
glassfit

#Out of sample deviance across CV folds for multinomial logistic regression on glass shard data
plot(glassfit)

par(mfrow=c(2,3), mai=c(.6,.6,.4,.4))
#Regularization paths for glmnet multinomial logistic lasso regression.
plot(glassfit$glm, xvar="lambda")

B <- coef(glassfit, select="min"); ## extract coefficients
B <- do.call(cbind, B)  #delete the column names
colnames(B) <- levels(gtype)# put column names again
B

head(fgl)
DeltaBMg <- B["Mg", "WinNF"] - B["Mg", "WinF"]; 
DeltaBMg # k is Mg, a is WinNF, b is WinF. 
exp(DeltaBMg)
1 - exp(DeltaBMg)
#Interpretation : 1 unit increase in Mg decreases odds of nonfloat glass over
#float glass by 0.36.

DeltaBNa <- B["Na", "Veh"] - B["Na", "Con"]; 
DeltaBNa # k is Mg, a is WinNF, b is WinF. 
exp(DeltaBNa)
exp(DeltaBNa)-1

#Interpretation : 1 unit increase in Na increases odds of vehicle window over
#container by 0.24.

probfgl <- predict(glassfit, xfgl, type="response")
dim(probfgl)
head(probfgl,n=2)
tail(probfgl,n=2)

#gives in-sample probabilities. Note: this is nXKX1 array. Need nXK array. To convert:
probfgl <- drop(probfgl) #use dim(probfgl) to check dim is 214 by 6
n <- nrow(xfgl)
trueclassprobs <- probfgl[cbind(1:n, gtype)]
head(trueclassprobs,n=3)
tail(trueclassprobs,n=3)

plot(trueclassprobs ~ gtype, col="lavender", varwidth=TRUE,
     xlab="glass type", ylab="prob( true class )") 
