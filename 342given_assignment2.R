# Prob 3.3



P083.fit1 = lm(F~P1,data=P083)

summary(P083.fit1)



P083.fit2 = lm(F~P2,data=P083)

summary(P083.fit2)



P083.fit3 = lm(F~P1+P2,data=P083)

summary(P083.fit3)



# Prob 3.15

P088.fit = lm(Sales ~ Age+HS+Income+Black+Female+Price,data=P088)

summary(P088.fit)



rm.fit.b = lm(Sales ~ Age+Income+Black+Price,data=P088)

summary(rm.fit.b)



anova(rm.fit.b,P088.fit)



confint(P088.fit,level=0.95)



rm.fit.d = lm(Sales ~ Age+HS+Black+Female+Price,data=P088)

summary(rm.fit.d)



rm.fit.e = lm(Sales ~ Age+Income+Price,data=P088)

summary(rm.fit.e)



rm.fit.f = lm(Sales ~ Income,data=P088)

summary(rm.fit.f)



# Prob 4.7

pairs(P088)

P088.cor = cor(P088[c("Age","HS","Income","Black","Female","Price","Sales" )])

corrplot(P088.cor,method="number",diag=TRUE,type="full")



# Prob 4.8

e = as.vector(residuals(P083.fit1))

d = e/sqrt(516.34)

p = as.vector(hatvalues(P083.fit1))

po = p/(1-p);

re = ((p+1)/(1-p))*(d**2/(1-d**2));

plot(re,po,pch=19,cex=1, main="Model 1(P1)")

text(re,po,labels=rownames(P083),pos=4,col="red",cex=1,font=1)



e = as.vector(residuals(P083.fit2))

d = e/sqrt(365.46)

p = as.vector(hatvalues(P083.fit2))

po = p/(1-p);

re = ((p+1)/(1-p))*(d**2/(1-d**2));

plot(re,po,pch=19,cex=1, main="Model 2(P2)")

text(re,po,labels=rownames(P083),pos=4,col="red",cex=1,font=1)



e = as.vector(residuals(P083.fit3))

d = e/sqrt(296.83)

p = as.vector(hatvalues(P083.fit3))

po = p/(1-p);

re = ((p+1)/(1-p))*(d**2/(1-d**2));

plot(re,po,pch=19,cex=1, main="Model 1(P1 & P2)")

text(re,po,labels=rownames(P083),pos=4,col="red",cex=1,font=1)



P083.a = P083[which((rownames(P083) != "15") &(rownames(P083) != "7") & (rownames(P083) != "9")),]



P083.fit.a = lm(F~P1+P2,data=P083.a)

summary(P083.fit.a)



# Prob 4.12

par(mfrow=c(1,1))

P128.cor = cor(P128[c("Y","X1","X2","X3","X4","X5","X6")])

corrplot(P128.cor,method="number",diag=TRUE,type="full")



P128.fit = lm(Y~X1+X2+X3+X4+X5+X6,data=P128)

summary(P128.fit)



Fitted = as.vector(fitted(P128.fit))

Residual = as.vector(residuals(P128.fit))

Rstandard = as.vector(rstandard(P128.fit))

Rstudent = as.vector(rstudent(P128.fit))

P128.pred = data.frame(P128,Fitted,Residual,Rstandard,Rstudent)

P128.pred



par(mfrow=c(2,3))

plot(P128.fit,which=1,col=c("blue")) # Residuals vs Fitted Plot

plot(P128.fit,which=2,col=c("red"))  # Q-Q Plot

plot(P128.fit,which=3,col=c("blue"))  # Scale-Location Plot



plot(P128.fit,which=4,col=c("blue"))  # Cook's Distance

plot(P128.fit,which=5,col=c("blue"))  # Residuals vs Leverage

plot(P128.fit,which=6,col=c("blue"))  # Cook's D vs Leverage



influence.measures(P128.fit)



plot(obs.num,Residual,pch=19,cex=1) 



Cooks.distance = as.vector(cooks.distance(P128.fit))

plot(obs.num,Cooks.distance,pch=19,cex=1) 



Dffits = as.vector(dffits(P128.fit))

plot(obs.num,Dffits,pch=19,cex=1) 

abline(h=c(-0.67,0,0.67),lty=2)



X <- as.matrix(cbind(c(rep(1,40)),P128.fit$model[,2:7]))

p <- diag(X%*%solve(t(X)%*%X)%*%t(X))

d <- P128.fit$residuals/sqrt(sum(P128.fit$residuals^2))


po = p/(1-p)

re = ((6+1)/(1-p))*(d**2/(1-d**2))

H  = po + re

plot(obs.num,H,pch=19,cex=1)

plot(re, po, pch=19,cex=1)

text(re, po, labels=rownames(P128), pos=4, col="red", cex=1, font=1)


#solution
P083.fit1 = lm(F~P1,data=P083)
summary(P083.fit1)

P083.fit2 = lm(F̃~P2,data=P083)
summary(P083.fit2)

P083.fit3 = lm(F̃~P1+P2,data=P083)
summary(P083.fit3)
