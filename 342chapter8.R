##############################################################################
## 소비자 지출액과 통화량 데이터 #############################################
##############################################################################
P211.fit = lm(Expenditure~Stock,data=P211)
summary(P211.fit)

par(mfrow=c(2,3))
plot(P211.fit,which=1,col=c("blue")) # Residuals vs Fitted Plot
plot(P211.fit,which=2,col=c("red"))  # Q-Q Plot
plot(P211.fit,which=3,col=c("blue"))  # Scale-Location Plot

plot(P211.fit,which=4,col=c("blue"))  # Cook's Distance
plot(P211.fit,which=5,col=c("blue"))  # Residuals vs Leverage
plot(P211.fit,which=6,col=c("blue"))  # Cook's D vs Leverage

par(mfrow=c(1,1))
plot(P211.fit$residuals ,type = "b", ylab="Residual", main="Index Plot",
     cex.axis = 1.5,cex.lab = 1.5,font=2, cex.main=1.5)
abline(h=0)

library(car)
library(DescTools)

# Run Test
RunsTest(P211.fit$residuals)

# perform Durbin-Watson test
durbinWatsonTest(P211.fit)

# 변환을 통한 자기 상관의 제거 (1회)
library(dplyr)
P211$TExpend = P211$Expenditure - 0.7506122*lag(P211$Expenditure)
P211$TStock  = P211$Stock       - 0.7506122*lag(P211$Stock)

P211T.fit = lm(TExpend~TStock,data=P211)
summary(P211T.fit)
durbinWatsonTest(P211T.fit)

plot(P211T.fit$residuals ,type = "b", ylab="Residual", main="Index Plot",
     cex.axis = 1.5,cex.lab = 1.5,font=2, cex.main=1.5)
abline(h=0)

# Cochrane-Orcutt 추정법
library(orcutt)
P211.fit.co <- cochrane.orcutt(lm(Expenditure ~ Stock, data=P211), convergence = 8, max.iter=100)

P211.fit.co$rho
summary(P211.fit.co)

##############################################################################
## 주택착공 데이터               #############################################
##############################################################################

pairs(P219)
lm(H~P, data=P219)

P219.fit <- lm(H~P, data=P219)
durbinWatsonTest(P219.fit)
plot(P219.fit$residuals ,type = "b", ylab="Residual", main="Index Plot",
       cex.axis = 1.5,cex.lab = 1.5,font=2, cex.main=1.5)
abline(h=0)

P219PD.fit <- lm(H~P+D, data=P219)
summary(P219PD.fit)
durbinWatsonTest(P219PD.fit)
plot(P219PD.fit$residuals ,type = "b", ylab="Residual", main="Index Plot",
     cex.axis = 1.5,cex.lab = 1.5,font=2, cex.main=1.5)
abline(h=0)

##############################################################################
## 스키 판매액 대 가처분소득     #############################################
##############################################################################

plot(P224$PDI,P224$Sales, ylab="Sales", xlab="PDI")
P224.fit <- lm(Sales~PDI, data=P224)
summary(P224.fit)

P224.fit$QTR <- as.numeric( substr(P224$Quarter, 2, 2))
P224.fit$ID <- 1:40

plot(P224.fit$ID ,P224.fit$residuals,xlab="ID", ylab="Residuals", type="n")
points(P224.fit$ID,P224.fit$residuals,xlab="ID", ylab="Residuals", pch = ifelse( ((1< P224.fit$QTR) & (P224.fit$QTR< 4)),16,1))

durbinWatsonTest(P224.fit)

P224$QTR <- as.numeric( substr(P224$Quarter, 2, 2))
P224$season <- ifelse( ((1< P224$QTR) & (P224$QTR< 4)),0,1) 

P224z.fit <- lm(Sales~PDI+season, data=P224)
summary(P224z.fit)

