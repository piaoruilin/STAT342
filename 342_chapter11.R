#######################################
# Supervisor Performance Data
#######################################

P060.fit <- lm(formula = Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = P060)
summary(P060.fit)

library(olsrr)

ols_vif_tol(P060.fit)
ols_eigen_cindex(P060.fit)

cor.X <- cor(P060[,2:7])
lambda<-eigen(cor.X)
lambda

# All possible models.
k<-ols_step_all_possible(P060.fit)
k$result$cp
k$result$aic


plot(k$result$n[k$result$cp<10],k$result$cp[k$result$cp<10],xlab="p",ylab="C_p")

# Forward selection.
ols_step_forward_p(P060.fit)

# Backward elimination.
ols_step_backward_p(P060.fit)

#######################################
# Homicide Data
#######################################

P315.S <- data.frame(scale(P315))

P315.fit.S <- lm(H ~ G + M + W, data = P315.S)
summary(P315.fit.S)

library(olsrr)

ols_vif_tol(P315.fit.S)

DX<-cbind(P315.S$G,P315.S$M,P315.S$W)
eigen(cor(DX))

# All possible models.
ols_step_all_possible(P315.fit.S)

# Forward selection.
ols_step_forward_p(P315.fit.S)

# Backward elimination.
ols_step_backward_p(P315.fit.S)

#################################
# Air pollution data
#################################

attach(P320)
eigen(cor(cbind(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15)))
detach(P320)

P320.S <- data.frame(cbind(apply(P320[,1:16],2,scale),P320$Y))
P320.fit <- lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15,data=P320.S)
summary(P320.fit)

ols_vif_tol(P320.fit)

library(MASS)
library(lmridge)

mod <- lmridge(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15 -1, P320.S, K = seq(0, 0.5, 1e-3) )
cc<-coefficients(mod)

plot(mod$K,cc[,1],type="l",lty=1,xlab="k",ylab="Theta",xlim=c(0.0,0.5),ylim=c(-0.4,0.8))
lines(mod$K,cc[,2],type="l",lty=2)
lines(mod$K,cc[,3],type="l",lty=3)
lines(mod$K,cc[,4],type="l",lty=4)
lines(mod$K,cc[,5],type="l",lty=5)
legend(x = "topright",  legend = c("1", "2", "3", "4", "5"),  lty = c(1, 2, 3, 4, 5))

plot(mod$K ,cc[,6],type="l",lty=1,xlab="k",ylab="Theta",xlim=c(0.0,0.5),ylim=c(-0.4,0.8))
lines(mod$K,cc[,7],type="l",lty=2)
lines(mod$K,cc[,8],type="l",lty=3)
lines(mod$K,cc[,9],type="l",lty=4)
lines(mod$K,cc[,10],type="l",lty=5)
legend(x = "topright",  legend = c("6", "7", "8", "9", "10"),  lty = c(1, 2, 3, 4, 5))

plot(mod$K ,cc[,11],type="l",lty=1,xlab="k",ylab="Theta",xlim=c(0.0,0.5),ylim=c(-0.4,0.8))
lines(mod$K,cc[,12],type="l",lty=2)
lines(mod$K,cc[,13],type="l",lty=3)
lines(mod$K,cc[,14],type="l",lty=4)
lines(mod$K,cc[,15],type="l",lty=5)
legend(x = "topright",  legend = c("11", "12", "13", "14", "15"),  lty = c(1, 2, 3, 4, 5))

P320.A.fit <- lm(Y~X1+X2+X3+X4+X5+X6+X9+X12+X13+X14,data=P320.S)
summary(P320.A.fit)

ols_vif_tol(P320.A.fit)

mod.a <-  lmridge(Y~X1+X2+X3+X4+X5+X6+X9+X12+X13+X14 -1, P320.S, K = seq(0, 0.5, 1e-3) )
cc<-coefficients(mod.a)

par(mfrow=c(2,1))
plot(mod.a$K, cc[,1],type="l",lty=1,xlab="k",ylab="Theta",xlim=c(0.0,0.5),ylim=c(-0.6,0.6))
lines(mod.a$K,cc[,2],type="l",lty=2)
lines(mod.a$K,cc[,3],type="l",lty=3)
lines(mod.a$K,cc[,4],type="l",lty=4)
lines(mod.a$K,cc[,5],type="l",lty=5)
legend(x = "topright",  legend = c("1", "2", "3", "4", "5"),  lty = c(1, 2, 3, 4, 5))

plot(mod.a$K, cc[,6],type="l",lty=1,xlab="k",ylab="Theta",xlim=c(0.0,0.5),ylim=c(-0.6,0.6))
lines(mod.a$K,cc[,7],type="l",lty=2)
lines(mod.a$K,cc[,8],type="l",lty=3)
lines(mod.a$K,cc[,9],type="l",lty=4)
lines(mod.a$K,cc[,10],type="l",lty=5)
legend(x = "topright",  legend = c("6", "9", "12", "13", "14"),  lty = c(1, 2, 3, 4, 5))

P320.B.fit <- lm(Y~X1+X2+X3+X4+X5+X6+X9+X14,data=P320.S)
summary(P320.B.fit)

ols_vif_tol(P320.B.fit)
