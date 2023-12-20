# Classification

# In this section I learned the algorithm of KNN and limitations of this method.
#Also, I learned how to use the rule when calculating FPR and FNR and TPR, TNR. 
#I learned the dependencies of these ratio from the classification rule. Also, 
#the expected loss can be attached for labels with different strength. Also, 
#I learned how to build the ROC curve for in sample and out of sample data to 
#present the performance of the model. Besides, I studied multinomial logistic 
#regression and how to interpret the coefficients and dependence of the increase 
#in 1 class on the change in another class.

library(MASS)
library(class) # load to have a knn function
data(fgl)

#first look at the data
head(fgl, n = 2)

#plot the distribution of the the elemental composition of shards by glass type
par(mfrow=c(2,3))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6), las=2) # mostly presented by window of all types
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6), las=2) # mostly presented by Vehicle headlamp
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6), las=2)

#to run KNN we need to scale variables to be able to measure the distance between them
#if we scale the whole data set then we have an error because the 10th column does not include numbers.
x <- scale(fgl[,1:10])# error

#we scale explanatory variable before measuring distances on the 1:9 columns
x <- scale(fgl[,1:9])

#we used the units of standard deviation in scale function. Therefore if we calculate the sd of scaled data we should receive 1
apply(x,2,sd)

#create a random sample with 10 rows 
test <- sample(1:nrow(fgl),10) #draw a random sample of 10 rows

#create knn with 1 number of neighbors considered
nearest1 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=2)# cl - factor of true classifications of training set

#create knn with 5 number of neighbors considered
nearest5 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=5)

data.frame(fgl$type[test],nearest1,nearest5) # create a data frame with predicted classes and true label

Acuuracy1<- mean(fgl$type[test]==nearest1)
Acuuracy5<- mean(fgl$type[test]==nearest5)

data.frame(Acuuracy1,Acuuracy5)
#the accuracy for the nearest 5 neighbours is lower that for the 1 neighbour 
#because the model capture more data that can influence the prediction of the true label. 
#However the results will change every time when running KNN

#Probability, Cost, and Classification part

credit<- read.csv("C://Users//Диана//Desktop//Master's degree//block 2//DS in HR//week_4//credit.csv")#data set

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
source('naref.R')
credx <- sparse.model.matrix(Default ~ . ^ 2, data=naref(credit)) 
colnames(credx)

default<- credit$Default
credscore<- cv.gamlr(credx,default,family='binomial')

par(mfrow=c(1,2))
plot(credscore$gamlr) # the resulting regularization path
plot(credscore)#OOS prediction performance. 

sum(coef(credscore, s="min")!=0) #how many coefficients are not equal to 0 = are significant for the model

sum(coef(credscore$gamlr, s=which.min(AIC(credscore$gamlr)))!=0) #how many coefficients are not equal to 0 based on the AIC

# the OOS R^2
1 - credscore$cvm[credscore$seg.min]/credscore$cvm[1]

## we can get probabilities of default
pred <- predict(credscore$gamlr, credx, type="response")
pred <- drop(pred) # remove the sparse Matrix formatting

boxplot(pred ~ default, xlab="default", ylab="prob of default", col=c("pink","dodgerblue")) #plot in-sample fit of the model


rule <- 1/5
sum( (pred>rule)[default==1] )/sum(pred>rule) ## false positive rate at 1/5 rule

sum( (pred<rule)[default==0] )/sum(pred<rule) ## false negative rate at 1/5 rule

#when we changed the rule from 1/5 to 2/5 the  main change was in false negative rate (from 92 to 21) because we ut the main cost on default
rule <- 2/5
sum( (pred>rule)[default==0] )/sum(pred>rule) ## false positive rate at 2/5 rule

sum( (pred<rule)[default==1] )/sum(pred<rule) ## false negative rate at 2/5 rule

#the rule 2/5 specific (specificity is equal to 88%) but not sensitive (sensitivity is equal to 45%)
sum( (pred>rule)[default==1] )/sum(default==1) ## sensitivity at 2/5 rule
sum( (pred<rule)[default==0] )/sum(default==0) ## specificity at 2/5 rule


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


#Multinomial Logistic Regression

library(glmnet)
xfgl <- sparse.model.matrix(type~.*RI, data=fgl)[,-1] #Design matrix includes chemical composition variable
gtype <- fgl$type
glassfit <- cv.glmnet(xfgl, gtype, family="multinomial") #cross validation experiments
glassfit

#Out of sample deviance across CV folds for multinomial logistic regression on glass shard data.
#We see that the lowest OOS deviance is around 2.0 and the null model deviance is around 3.0. The R^2 then is 1-2/3=1/3
plot(glassfit)

#plot Lasso regularization paths for multinomial logistic lasso regression on the glass data
par(mfrow=c(2,3), mai=c(.6,.6,.4,.4))
plot(glassfit$glm, xvar="lambda") # there are 6 plots for every class label in gtype

B <- coef(glassfit, select="min"); # extract coefficients based on the lambda min

#use do.call function to make a table from small tables of coefficients related
#to separated gtype (we use cbind function inside the do.call function)
B <- do.call(cbind, B)  

# put column names in a table
colnames(B) <- levels(gtype)


head(fgl)
#calculating the difference in slope of coefficients of classes MG, WinNF, and WinF to 
#estimate the influence of MG class on WinNF class and WinF

DeltaBMg <- B["Mg", "WinNF"] - B["Mg", "WinF"] # based on the delta beta function -> k is Mg, a is WinNF, b is WinF.
 
#as we have negative delta beta, we need to estimate odds of decrease as 1-(exp(b delta))
1 - exp(DeltaBMg)# 1 unit increase in Mg decreases odds of nonfloat glass (WinNF) 
#over float glass (WinF) by 0.36.

#another example of calculationg beta delta
DeltaBNa <- B["Na", "Veh"] - B["Na", "Con"] #  k is NA, a is Veh, b is Con
DeltaBNa

#as we have positive delta beta, we need to estimate odds of decrease as (exp(b delta))-1
exp(DeltaBNa)-1 # 1 unit increase in Na increases odds of vehicle window (Veh) 
#over container (Con) by 0.37.

#as we have positive delta beta, we need to estimate odds of decrease as (exp(b delta))-1
exp(DeltaBNa)-1 # 1 unit increase in Na increases odds of vehicle window (Veh) 
#over container (Con) by 0.37.

