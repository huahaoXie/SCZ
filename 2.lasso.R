
setwd("C:/Users/Administrator/Desktop/WORK/2.bl-pr/2.analysis/3.lasso")               


data=read.csv("data.csv",header = T)         
head(data)


library(glmnet)
library(foreign)



y<-as.matrix(data[,2])
x<-as.matrix(data[,c(6:80)])

f1 = glmnet(x, y, family="binomial", nlambda=100, alpha=1) 

print(f1)

plot(f1, xvar="lambda", label=TRUE)


cvfit=cv.glmnet(x,y)
plot(cvfit)



cvfit$lambda.min
cvfit$lambda.1se #求出最小值一个标准误的λ值


l.coef2<-coef(cvfit$glmnet.fit,s=0.006817023,exact = F)
l.coef1<-coef(cvfit$glmnet.fit,s=0.03314847,exact = F)
l.coef1
l.coef2


exp(confint(mod)) 

exp(coef(mod))    