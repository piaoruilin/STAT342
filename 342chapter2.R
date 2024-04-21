load("/Users/piaoruilin/Desktop/DATASCIENCE/STAT342/RABE5.RData")

install.packages("car")
library(car) 

# Page 9 Example
plot(Y~X,data=P029a,type="p",pch=20,col="blue",cex=2)
car::scatterplot(Y~X,data=P029a,pch=1,cex=2)
cor(P029a[c("Y","X")],method=c("pearson"))

# Page 11 Anscombe’s Quartet Example
par(mfrow=c(2,2))
plot(Y1~X1,data=P029b,type="p",pch=20,col="blue",cex=2,xlim=c(0,20),ylim=c(0,14))
abline(lm(Y1~X1,data=P029b),lwd=2,col="red")
plot(Y2~X2,data=P029b,type="p",pch=20,col="blue",cex=2,xlim=c(0,20),ylim=c(0,14))
abline(lm(Y2~X2,data=P029b),lwd=2,col="red")
plot(Y3~X3,data=P029b,type="p",pch=20,col="blue",cex=2,xlim=c(0,20),ylim=c(0,14))
abline(lm(Y3~X3,data=P029b),lwd=2,col="red")
plot(Y4~X4,data=P029b,type="p",pch=20,col="blue",cex=2,xlim=c(0,20),ylim=c(0,14))
abline(lm(Y4~X4,data=P029b),lwd=2,col="red")

# Page 15 Computer repair time Example
par(mfrow=c(1,1))
plot(Minutes~Units,data=P031,type="p",pch=20,col="blue",cex=2)
abline(lm(Minutes~Units,data=P031),lwd=2,col="red")
cor(P031[c("Minutes","Units")],method=c("pearson"))
cor.test(~Minutes+Units,data=P031,method=c("pearson"))

# Page 19 Computer repair time Example
car::scatterplot(Minutes~Units,data=P031,cex=3,lwd=2)
P031.fit = lm(Minutes~Units,data=P031)
summary(P031.fit)

# Page 26 Computer repair time Example
summary(P031.fit)

# Page 28 Computer repair time Example
coef(summary(P031.fit))
tstat = (coef(summary(P031.fit))[2,1]-12)/coef(summary(P031.fit))[2,2]
tstat
df = P031.fit$df.residual
df
qt(0.025,df,lower.tail=FALSE)
2*pt(tstat,df,lower.tail=FALSE)

# Page 29 Computer repair time Example
cor.test(~Minutes+Units,data=P031,method=c("pearson"))

# Page 31 Prediction Limit
P031.pred = predict(P031.fit,interval='prediction',level=0.95)
P031.pred = data.frame(P031,P031.pred)
P031.pred

# Page 32 Confidence Limit
P031.conf = predict(P031.fit,interval='confidence',level=0.95)
P031.conf = data.frame(P031,P031.conf)
P031.conf

# Page 36 Coefficient of Determination
plot(fit~Minutes,data=P031.pred,type="p",pch=20,col="blue",cex=2)
abline(lm(fit~Minutes,data=P031.pred),lwd=2,col="red")
cor(P031.pred[c("Minutes","fit")],method=c("pearson"))
P031.aov = aov(Minutes~Units,data=P031)
summary(P031.aov)


