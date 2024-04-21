# p7: Hamilton Data 

P103.fit = lm(Y~X1+X2,data=P103)
summary(P103.fit)

Fitted = as.vector(fitted(P103.fit))
Residual = as.vector(residuals(P103.fit))
Rstandard = as.vector(rstandard(P103.fit))
Rstudent = as.vector(rstudent(P103.fit))
P103.pred = data.frame(P103,Fitted,Residual,Rstandard,Rstudent)
P103.pred

# p12: Hamilton Data (일차원 그래프)
hist(P103$Y,breaks=seq(10,14,by=0.5),freq=TRUE,density=5,col="red")
hist(P103$X1,breaks=seq(2,4,by=0.4),freq=TRUE,density=5,col="blue")
hist(P103$X2,breaks=seq(4,10,by=1),freq=TRUE,density=5,col="blue")

stem(P103$Y,scale=1)
stem(P103$X1,scale=1)
stem(P103$X2,scale=2)

boxplot(P103$Y,col="gray",horizontal=TRUE,notch=TRUE)
boxplot(P103$X1,col="orange",horizontal=TRUE,notch=FALSE)
boxplot(P103$X2,col="orange",horizontal=TRUE,notch=FALSE)

install.packages("DescTools")
library(DescTools)
Desc(P103$Y,plotit=TRUE)
Desc(P103$X1,plotit=TRUE)
Desc(P103$X2,plotit=TRUE)

# p12: Hamilton Data (이차원 그래프)
plot(P103[c("Y","X1","X2")],pch=9,cex=2)
library(car)
scatterplotMatrix(P103[c("Y","X1","X2")],pch=1,cex=1,ellipse=TRUE)

install.packages("corrplot")
library(corrplot)
P103.cor = cor(P103[c("Y","X1","X2")])
corrplot(P103.cor,method="number",diag=TRUE,type="full")
corrplot(P103.cor,method="ellipse",diag=TRUE,type="full")

# p15: Hamilton Data (잔차 플롯)
plot(P103.fit,which=1,col=c("blue")) # Residuals vs Fitted Plot
plot(P103.fit,which=2,col=c("red"))  # Q-Q Plot
plot(P103.fit,which=3,col=c("blue"))  # Scale-Location Plot

plot(P103.fit,which=4,col=c("blue"))  # Cook's Distance
plot(P103.fit,which=5,col=c("blue"))  # Residuals vs Leverage
plot(P103.fit,which=6,col=c("blue"))  # Cook's D vs Leverage

#######################
# New York River data #
#######################

# Scatter Plot
plot(P010[c("Agr","Forest","Rsdntial","ComIndl","Nitrogen")],pch=1,cex=2)

# Regression analysis
P010.fit = lm(Nitrogen~Agr+Forest+Rsdntial+ComIndl,data=P010)
summary(P010.fit)

# Fitted value and residual calculation 
Fitted = as.vector(fitted(P010.fit))
Residual = as.vector(residuals(P010.fit))
Rstandard = as.vector(rstandard(P010.fit))
Rstudent = as.vector(rstudent(P010.fit))
P010.pred = data.frame(P010,Fitted,Residual,Rstandard,Rstudent)
P010.pred

# Residual plots
plot(Rstudent~Fitted,data=P010.pred,pch=19,cex=1) 
abline(h=-3:3,lty=2)

# Leverage plot
River = rownames(P010)
River[19]="Oswegatchie"
P010_1 <- cbind(River,P010)

obs.num = 1:20
Hatvalues = as.vector(hatvalues(P010.fit))
plot(obs.num,Hatvalues,pch=19,cex=1) 
abline(h=0.5,lty=2)

# p20: t-test result after deleting "Neversink" or "Hackensack"
P010.a = P010_1[which(P010_1$River != "Neversink"),]
P010.a.fit = lm(Nitrogen~Agr+Forest+Rsdntial+ComIndl,data=P010.a)
summary(P010.a.fit)

P010.b = P010_1[which(P010_1$River != "Hackensack"),]
P010.b.fit = lm(Nitrogen~Agr+Forest+Rsdntial+ComIndl,data=P010.b)
summary(P010.b.fit)

# p19: Masking & Swamping
plot(Nitrogen~ComIndl,data=P010,pch=19,cex=1)
abline(lm(Nitrogen~ComIndl,data=P010),lwd=2,col="blue")
text(Nitrogen~ComIndl,labels=rownames(P010),data=P010,pos=4,col="red",cex=1,font=1)

# Nitroge = Comlndl 모형에서의 잔차 플롯 
P010.X4.fit = lm(Nitrogen~ComIndl,data=P010)
summary(P010.X4.fit)

Rstandard = as.vector(rstandard(P010.X4.fit))
Hatvalues = as.vector(hatvalues(P010.X4.fit))
P010.X4.pred = data.frame(P010[c("Nitrogen","ComIndl")],Rstandard,Hatvalues)
P010.X4.pred

plot(obs.num,P010.X4.pred$Rstandard,pch=19,cex=1) 
abline(h=-3:3,lty=2)

plot(obs.num,P010.X4.pred$Hatvalues,pch=19,cex=1) 
abline(h=0.2,lty=2)

# 영향력 통계
influence.measures(P010.X4.fit)

Cooks.distance = as.vector(cooks.distance(P010.X4.fit))
plot(obs.num,Cooks.distance,pch=19,cex=1) 

Dffits = as.vector(dffits(P010.X4.fit))
plot(obs.num,Dffits,pch=19,cex=1) 
abline(h=c(-0.67,0,0.67),lty=2)

X <- cbind(c(rep(1,20)),P010.X4.fit$model[,2])
p <- diag(X%*%solve(t(X)%*%X)%*%t(X))
d <-P010.X4.fit$residuals/sqrt(sum(P010.X4.fit$residuals^2))

po = p/(1-p);
re = ((1+1)/(1-p))*(d**2/(1-d**2));
plot(re,po,pch=19,cex=1)
text(re,po,labels=rownames(P010),pos=4,col="red",cex=1,font=1)

###############################
# Scottish Hills Races Data. ##
###############################

# Multiple regression model 
P120.fit = lm(Time~Distance+Climb,data=P120)
summary(P120.fit)

# added-variable plot
avPlots(P120.fit,id=TRUE)

# component plus residual plot
crPlots(P120.fit,id=TRUE)

# 잠재성-잔차플롯
e = as.vector(residuals(P120.fit))
d = e/sqrt(24810082)
p = as.vector(hatvalues(P120.fit))
po = p/(1-p);
re = ((2+1)/(1-p))*(d**2/(1-d**2));
plot(re,po,pch=19,cex=1)
text(re,po,labels=rownames(P010),pos=4,col="red",cex=1,font=1)

