##################################################
# 공장종업원과 감독자 데이터 #####################
##################################################

# p. 4 (변수변환에 의한 회귀분석)
P176$TY = P176$Y/P176$X
P176$TX = 1/P176$X
P176.A.fit = lm(TY~TX,data=P176)
summary(P176.A.fit)

Fitted.TY = as.vector(fitted(P176.A.fit))
Residual.TY = as.vector(residuals(P176.A.fit))
Fitted.Y = Fitted.TY*P176$X
Residual.Y = Residual.TY*P176$X 

sum((P176$Y-mean(P176$Y))**2)
sum((Residual.Y)**2)

# p. 4 (가중최소제곱법)

P176$Wgt = 1/(P176$X*P176$X)
P176.B.fit = lm(Y~X,data=P176,weights=Wgt)
summary(P176.B.fit)

Rstandard = as.vector(rstandard(P176.B.fit))
Fitted = fitted(P176.B.fit)
plot(Fitted,Rstandard,pch=19,cex=1) 

##################################################
# 교육비 지출 데이터         #####################
##################################################

# p. 9 - 10
P198.A.fit = lm(Y~X1+X2+X3,data=P198)
summary(P198.A.fit)

plot(P198.A.fit,which=1,col=c("blue")) # Residuals vs Fitted Plot

Fitted = fitted(P198.A.fit)
Rstandard = as.vector(rstandard(P198.A.fit))

plot(Fitted,Rstandard,pch=19,cex=1) 
plot(P198$Region,Rstandard,pch=19,cex=1)
plot(P198$X1,Rstandard,pch=19,cex=1)

# p. 11 - 12: 회귀분석(n=49, Alaska 주 제외)
P198.B = P198[which(row.names(P198)!=49),]
P198.B.fit = lm(Y~X1+X2+X3,data=P198.B)
summary(P198.B.fit)

Fitted = fitted(P198.B.fit)
Rstandard = as.vector(rstandard(P198.B.fit))

plot(Fitted,Rstandard,pch=19,cex=1) 
plot(P198.B$Region,Rstandard,pch=19,cex=1)

# p. 13 : 가중최소제곱 (n=49, Alaska 주 제외)
P198.C = P198.B
P198.C$R = as.vector(residuals(P198.B.fit))
P198.C$R2 = P198.C$R*P198.C$R
sum(!is.na(P198.C$R2))

sum(P198.C$R2)

install.packages("doBy")
library(doBy)
fun <- function(x){c(sum(!is.na(x)),sum(x))}
summaryBy(R2~Region,data=P198.C,FUN=fun)

P198.C$Wgt = 0
P198.C$Wgt[P198.C$Region==1] = 1/(1.177*1.177)
P198.C$Wgt[P198.C$Region==2] = 1/(1.503*1.503)
P198.C$Wgt[P198.C$Region==3] = 1/(0.475*0.475)
P198.C$Wgt[P198.C$Region==4] = 1/(0.938*0.938)
P198.C.fit = lm(Y~X1+X2+X3,data=P198.C,weights=Wgt)
summary(P198.C.fit)

plot(Fitted,Rstandard,pch=19,cex=1) 
plot(P198.C$Region,Rstandard,pch=19,cex=1)





















