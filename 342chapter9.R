P236.fit = lm(ACHV~FAM+PEER+SCHOOL,data=P236)
summary(P236.fit)

pairs(P236)
cor(P236)

P241.fit = lm(IMPORT ~ DOPROD+STOCK+CONSUM,data=P241)
summary(P241.fit)

plot(P241.fit$residuals,ylab="Residuals",type="o")

P241.fit1 = lm(IMPORT ~ DOPROD+STOCK+CONSUM,data=P241[P241$YEAR<=59,])
summary(P241.fit1)

plot(P241.fit1$residuals,ylab="Residuals",type="o")

newdata <- P241[P241$YEAR>59,]
cbind(P241[P241$YEAR>59,2],predict(P241.fit1, newdata, interval = "prediction"))

install.packages("olsrr")
library(olsrr)

P241.A <- P241[P241$YEAR<=59,]

ols_vif_tol(P241.fit)
ols_eigen_cindex(P241.fit)

cor.X <- cor(P241.A[,3:5])
lambda<-eigen(cor.X)
lambda
CI <- sort(sqrt(max(lambda$values)/(lambda$values)))
CI
