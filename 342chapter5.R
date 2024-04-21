# p5: 예: 급료조사 데이터 
P130$E1 = 0
P130$E1[P130$E==1] = 1
P130$E1[P130$E=='NA'] = 'NA'
P130$E2 = 0
P130$E2[P130$E==2] = 1
P130$E2[P130$E=='NA'] = 'NA'
head(P130,n=5)

# p7: 예: 급료조사 데이터 
P130.fit = lm(S~X+E1+E2+M,data=P130)
summary(P130.fit)

# p9: 예: 급료조사 데이터 
plot(P130.fit,which=1,col=c("blue"))

# p10: 범주별 잔차플롯
Fitted = as.vector(fitted(P130.fit))
Residual = as.vector(residuals(P130.fit))
Rstandard = as.vector(rstandard(P130.fit))
P130 = data.frame(P130,Fitted,Residual,Rstandard)

P130$Cat = 'NA'
P130$Cat[P130$E==1 & P130$M==0] = 1
P130$Cat[P130$E==1 & P130$M==1] = 2
P130$Cat[P130$E==2 & P130$M==0] = 3
P130$Cat[P130$E==2 & P130$M==1] = 4
P130$Cat[P130$E==3 & P130$M==0] = 5
P130$Cat[P130$E==3 & P130$M==1] = 6

plot(Rstandard~Cat,data=P130,pch=19,cex=1) 

# 요인 변수 사용 결과 (동일)[]
P130$E = factor(P130$E)
P130.fit = lm(S~X+E+M,data=P130)
summary(P130.fit) 

# p12: 상호작용 변수 
P130$E1 = as.numeric(P130$E1)
P130$E2 = as.numeric(P130$E2)
P130$M = as.numeric(P130$M)

P130$EM1 = P130$E1*P130$M
P130$EM2 = P130$E2*P130$M

P130.fit.a = lm(S~X+E1+E2+M+EM1+EM2,data=P130)
summary(P130.fit.a)

# p13: 상호작용 변수 (obs 33 제외)
P130.b = P130[c("S","X","E","M","E1","E2","EM1","EM2","Cat")]
P130.b = P130.b[which(row.names(P130.b)!=33),]
P130.fit.b = lm(S~X+E1+E2+M+EM1+EM2,data=P130.b)
summary(P130.fit.b)

# p14: 상호작용 변수 (obs 33 제외)
Fitted = as.vector(fitted(P130.fit.b))
Residual = as.vector(residuals(P130.fit.b))
Rstandard = as.vector(rstandard(P130.fit.b))
P130.b = data.frame(P130.b,Fitted,Residual,Rstandard)

plot(Rstandard~X,data=P130.b,pch=19,cex=1)
plot(Rstandard~Cat,data=P130.b,pch=19,cex=1)

# p15: 비가법모형을 사용한 기본 급료 추정
X = c(0,0,0,0,0,0)
E = c(1,1,2,2,3,3)
M = c(0,1,0,1,0,1)
P130.temp = data.frame(X,E,M)
P130.temp$E1 = 0
P130.temp$E1[P130.temp$E==1] = 1
P130.temp$E2 = 0
P130.temp$E2[P130.temp$E==2] = 1
P130.temp$EM1 = P130.temp$E1*P130.temp$M
P130.temp$EM2 = P130.temp$E2*P130.temp$M

predict(P130.fit.b,newdata=P130.temp,interval='prediction',level=0.95)
predict(P130.fit.b,newdata=P130.temp,interval='confidence',level=0.95)

# p19: 가설 검정(gamma=delta=0)
P140$RaceTest = P140$RACE*P140$TEST

P140.fit1 = lm(JPERF~TEST,data=P140)
summary(P140.fit1)

P140.fit2 = lm(JPERF~TEST+RACE+RaceTest,data=P140)
summary(P140.fit2)

# p20 
anova(P140.fit1,P140.fit2)

# p21: residual plot 
Fitted1 = as.vector(fitted(P140.fit1))
Residual1 = as.vector(residuals(P140.fit1))
Rstandard1 = as.vector(rstandard(P140.fit1))
P140.1 = data.frame(P140,Fitted1,Residual1,Rstandard1)

plot(Rstandard1~TEST,data=P140.1,pch=19,cex=1)
plot(Rstandard1~RACE,data=P140.1,pch=19,cex=1)

Fitted2 = as.vector(fitted(P140.fit2))
Residual2 = as.vector(residuals(P140.fit2))
Rstandard2 = as.vector(rstandard(P140.fit2))
P140.2 = data.frame(P140,Fitted2,Residual2,Rstandard2)

plot(Rstandard2~TEST,data=P140.2,pch=19,cex=1)
plot(Rstandard2~RACE,data=P140.2,pch=19,cex=1)

# p22: 집단별 회귀분석
P140.a = P140[which(P140$RACE==1),]
P140.fit.a = lm(JPERF~TEST,data=P140.a)
summary(P140.fit.a)

P140.b = P140[which(P140$RACE==0),]
P140.fit.b = lm(JPERF~TEST,data=P140.b)
summary(P140.fit.b)

# p25: same slope & different intercept 
P140.fit3 = lm(JPERF~TEST+RACE,data=P140)
summary(P140.fit3)

anova(P140.fit1,P140.fit3)

# p27: different slope & same intercept 
P140.fit4 = lm(JPERF~TEST+RaceTest,data=P140)
summary(P140.fit4)

anova(P140.fit1,P140.fit4)
