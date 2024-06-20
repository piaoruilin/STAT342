# Coleman et al IMPORT data 1949 - 1959

P241.A   <- P241[P241$YEAR<=59,] 
P241.STD <- data.frame(apply(P241.A[,-1],2,scale))
P241.COR <- cor(P241.STD[,-1])
P241.COR
eigen(P241.COR)
P241.PC <- prcomp(P241.STD[,-1], center=TRUE, scale.=TRUE)
P241.PC

# 광고비 데이터

P248.STD <- data.frame(apply(P248[,-1],2,scale))
P248.COR <- cor(P248.STD)
P248.COR

eigen(P248.COR)

P248.a <- data.frame(apply(P248,2,scale))
P248.fit.std <- lm(St ~ At+Pt+Et+At.1+Pt.1 -1 ,P248.a)
summary(P248.fit.std)

EV <- as.matrix(eigen(P248.COR)$vectors)%*%matrix(c(1,0,0,0,0,
                                                    0,-1,0,0,0,
                                                    0,0,-1,0,0,
                                                    0,0,0,-1,0,
                                                    0,0,0,0,1),5,5)

P248.b <- data.frame(cbind(P248.a$St,as.matrix(P248.STD)%*%EV))

names(P248.b) <- c("Y","C1","C2","C3","C4","C5")
P248.fit.prin <- lm(Y ~ C1+C2+C3+C4+C5 -1,P248.b)
summary(P248.fit.prin)

# Coleman et al IMPORT data 1949 - 1959

P241.A$NEWVAR <- P241.A$DOPROD+P241.A$CONSUM
P241.fit3 <- lm(IMPORT ~ STOCK+NEWVAR,P241.A)
summary(P241.fit3)

P241.fit1 = lm(IMPORT ~ DOPROD+STOCK+CONSUM,P241[P241$YEAR<=59,])
summary(P241.fit1)

cor2 <- cor(cbind(P241.A$STOCK,P241.A$NEWVAR))
cor2
eigen(cor2)

anova(P241.fit3,P241.fit1)

P241.fit4 = lm(IMPORT ~ DOPROD+STOCK,P241.A)
summary(P241.fit4)

P241.fit1 = lm(IMPORT ~ DOPROD+STOCK+CONSUM,P241[P241$YEAR<=59,])
summary(P241.fit1)

P241.fit2 = lm(IMPORT ~ DOPROD+STOCK+CONSUM,P241.STD)
summary(P241.fit2)

P241.PC.DAT <- data.frame(cbind(P241.A$IMPORT,P241.PC$x))
P241.fit.PC = lm(scale(V1) ~ PC1+PC2+PC3,P241.PC.DAT)
summary(P241.fit.PC)

summary(lm(scale(V1) ~ PC1,P241.PC.DAT))
summary(lm(scale(V1) ~ PC1+PC2,P241.PC.DAT))
summary(lm(scale(V1) ~ PC1+PC2+PC3,P241.PC.DAT))

# Hald의 데이터
U<-c(0.955, -0.746, -2.323, -0.820, 0.471,-0.299, 0.210,  0.558, -1.119,  0.496,  0.781,  0.918, 0.918)
C1<-c( 1.467, 2.136, -1.130,  0.660,-0.359, -0.967,-0.931,  2.232,  0.352, -1.663,  1.641, -1.693, -1.746)
C2<-c( 1.903,  0.238,  0.184,  1.577,  0.484,  0.170,-2.135, -0.692, -1.432, 1.828, -1.295, -0.392, -0.438)
C3<-c(-0.530, -0.290, -0.010, 0.179,-0.740, 0.086, -0.173,  0.460, -0.032,  0.851,  0.494, -0.020, -0.275)
C4<-c( 0.039, -0.030, -0.094, -0.033, 0.019, -0.012,  0.008,  0.023, -0.045,  0.020,  0.031,  0.037, 0.037)
P278.U <- data.frame(cbind(U,C1,C2,C3,C4)) 

P278.U.fit <- lm(U~C1+C2+C3+C4, data=P278.U)
summary(P278.U.fit)

P278.U.fit <- lm(U~C1+C2+C3, data=P278.U)
summary(P278.U.fit)

pairs(P278.U)

# Ridge regression

library(MASS)

P241.ridge.fit = lm.ridge(IMPORT ~ DOPROD+STOCK+CONSUM -1, P241.STD, lambda = seq(0, 1, 1e-3))
coef(P241.ridge.fit)

par(mar = c(4, 4, 4, 4), las = 1)
matplot(P241.ridge.fit$lambda, coef(P241.ridge.fit), main="Ridge Trace", type = "l", ylim = c(-0.5, 1.5),
        xlab = "k", ylab = expression(hat(theta)))
legend(0.8,1.5,c("Theta_1" , "Theta_2", "Theta_3"),lty=c(1,2,3),col=c("black","red","blue"),cex=1)

P241.fit1 = lm(IMPORT ~ DOPROD+STOCK+CONSUM,P241[P241$YEAR<=59,])
summary(P241.fit1)

P241.fit.std = lm(IMPORT ~ DOPROD+STOCK+CONSUM,P241.STD)
summary(P241.fit.std)

X <- as.matrix(P241.STD[,2:4])
Y  <- as.matrix(P241.STD[,1])

D <- diag(1/sqrt(diag(t(X)%*%X)))
Z <- X%*%D

theta.ridge.0.04 <- D%*%solve(t(Z)%*%Z+0.04*diag(rep(1,3)))%*%t(Z)%*%Y 

s <- sqrt(apply(P241.A,2,var))[2:5]
beta.ridge <- (s[1]/s[2:4])*theta.ridge.0.04

m <- apply(P241.A,2,mean)[2:5]
beta.ridge.0 <- m[1] - sum(m[2:4]*beta.ridge)