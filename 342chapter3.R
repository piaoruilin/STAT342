# p 6: 감독자 직무 수행능력 예     

P060.fit12 = lm(Y~X1+X2,data=P060)
summary(P060.fit12)

# p 12: 감독자 직무 수행능력 예  
P060.fit = lm(Y~X1+X2+X3+X4+X5+X6,data=P060)
summary(P060.fit)

# p 16: 감독자 직무 수행능력 예  
anova(P060.fit)

cor(P060$Y,fitted(P060.fit))^2 
ssr = sum((fitted(P060.fit)-mean(P060$Y))^2)
sse = sum((P060$Y-fitted(P060.fit))^2)
sst = sum((P060$Y-mean(P060$Y))^2)
ssr/sst
1-sse/sst

# p 18: 감독자 직무 수행능력 예 
P060.fit = lm(Y~X1+X2+X3+X4+X5+X6,data=P060)
summary(P060.fit)

# p 18: 감독자 직무 수행능력 예 
confint(P060.fit,level=0.95)

# p 25: 감독자 직무 수행능력 예 
rm.fit = lm(Y~1,data=P060)
fm.fit = lm(Y~X1+X2+X3+X4+X5+X6,data=P060)
anova(rm.fit,fm.fit)

# p 26: 감독자 직무 수행능력 예 
rm.fit = lm(Y~X1+X3,data=P060)
fm.fit = lm(Y~X1+X2+X3+X4+X5+X6,data=P060)
anova(rm.fit,fm.fit)

# p 27: 감독자 직무 수행능력 예 
P060$W = P060$X1+P060$X3
rm.fit = lm(Y~W,data=P060)
summary(rm.fit)

fm.fit = lm(Y~X1+X3,data=P060)
summary(fm.fit)
anova(rm.fit,fm.fit)

# p 27~28: 감독자 직무 수행능력 예 
P060$yprime = P060$Y-P060$X3
P060$V = P060$X1-P060$X3
yprime.fit = lm(yprime~V,data=P060)
summary(yprime.fit)

P060$yhat = fitted(yprime.fit)+P060$X3
# P060$yhat = 1.1665+0.6938*P060$X1+0.3062*P060$X3
rm.fit = lm(Y~yhat,data=P060)
summary(rm.fit)

fm.fit = lm(Y~X1+X3,data=P060)
summary(fm.fit)
anova(rm.fit,fm.fit)

# p 29: 감독자 직무 수행능력 예 
P060.fit = lm(Y~X1+X2+X3+X4+X5+X6,data=P060)
P060.pred = predict(P060.fit,interval='prediction',level=0.95)
P060.pred = data.frame(P060,P060.pred)
P060.pred

P060.fit = lm(Y~X1+X2+X3+X4+X5+X6,data=P060)
P060.conf = predict(P060.fit,interval='confidence',level=0.95)
P060.conf = data.frame(P060,P060.conf)
P060.conf






