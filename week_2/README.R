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
