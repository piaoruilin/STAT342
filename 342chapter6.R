##################################################
## X-선 방사에 의한 박테리아 사망률 ##############
##################################################

# p.10
plot(N_t~t,data=P168,pch=19,cex=1)
P168.fit = lm(N_t~t,data=P168)
summary(P168.fit)

Rstandard = as.vector(rstandard(P168.fit))
plot(P168$t,Rstandard,pch=19,cex=1) 

P168$logN_t = log(P168$N_t)
plot(logN_t~t,data=P168,pch=19,cex=1)

P168.A.fit = lm(logN_t~t,data=P168)
summary(P168.A.fit)

Rstandard = as.vector(rstandard(P168.A.fit))
plot(P168$t,Rstandard,pch=19,cex=1) 

confint(P168.A.fit,level=0.95)

##################################################
## 항공사 사고 데이터               ##############
##################################################

# p.16-17
plot(Y~N,data=P174,pch=19,cex=1)
P174.fit = lm(Y~N,data=P174)
summary(P174.fit)

Rstandard = as.vector(rstandard(P174.fit))
plot(P174$N,Rstandard,pch=19,cex=1) 

#p. 18
P174$sqrtY = sqrt(P174$Y)
plot(sqrtY~N,data=P174,pch=19,cex=1)
P174.A.fit = lm(sqrtY~N,data=P174)
summary(P174.A.fit)

Rstandard = as.vector(rstandard(P174.A.fit))
plot(P174$N,Rstandard,pch=19,cex=1)

##################################################
## 종업원 수와 감독자 수            ##############
##################################################

# p.19 - 20
plot(Y~X,data=P176,pch=19,cex=1)
P176.fit = lm(Y~X,data=P176)
summary(P176.fit)

Rstandard = as.vector(rstandard(P176.fit))
plot(P176$X,Rstandard,pch=19,cex=1)

# p.22
P176$TY = P176$Y/P176$X
P176$TX = 1/P176$X
plot(TY~TX,data=P176,pch=19,cex=1)

P176.A.fit = lm(TY~TX,data=P176)
summary(P176.A.fit)

# p.24 
P176$logY = log(P176$Y)
plot(logY~X,data=P176,pch=19,cex=1)

P176.B.fit = lm(logY~X,data=P176)
summary(P176.B.fit)

Rstandard = as.vector(rstandard(P176.B.fit))
plot(P176$X,Rstandard,pch=19,cex=1) 

# p. 25
P176$X2 = P176$X*P176$X
P176.C.fit = lm(logY~X+X2,data=P176)
summary(P176.C.fit)

Rstandard = as.vector(rstandard(P176.C.fit))
Fitted = fitted(P176.C.fit)
plot(Fitted,Rstandard,pch=19,cex=1) 

plot(P176$X,Rstandard,pch=19,cex=1) 
plot(P176$X2,Rstandard,pch=19,cex=1)

##################################################
## BrainWeight & BodyWeight         ##############
##################################################

# p.29 - 30
plot(BrainWeight~BodyWeight,data=P184,pch=19,cex=1)

attach(P184)
P184$tBrainWt1 = BrainWeight**.5
P184$tBodyWt1 = BodyWeight**.5
P184$tBrainWt2 = log(BrainWeight)  
P184$tBodyWt2 = log(BodyWeight)
P184$tBrainWt3 = BrainWeight**-.5  
P184$tBodyWt3 = BodyWeight**-.5
P184$tBrainWt4 = 1/BrainWeight
P184$tBodyWt4 = 1/BodyWeight
detach(P184)

plot(tBrainWt1~tBodyWt1,data=P184,pch=19,cex=1)
plot(tBrainWt2~tBodyWt2,data=P184,pch=19,cex=1)
plot(tBrainWt3~tBodyWt3,data=P184,pch=19,cex=1)
plot(tBrainWt4~tBodyWt4,data=P184,pch=19,cex=1)
