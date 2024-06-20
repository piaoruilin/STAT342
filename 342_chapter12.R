# Plot of Logit function 
x <- seq(-10,10,0.01)
y <- 1/(1+exp(-x)) 

par(mfrow=c(1,1))
plot(x, y,  xlab="X", type="l", ylab=expression(pi))

##############################################################
# Bankrupt example
##############################################################

# Logistic regression fit
P339.fit <- glm(Y~X1+X2+X3, family = binomial, data=P339)  
summary(P339.fit)

# Odds ratio
exp(cbind(Odds_Ratio = coef(P339.fit),confint.default(P339.fit)))[2:4,] 

# Diagnostic
P339.D <- data.frame(
  p = predict(P339.fit, type = "response"),   # 예측 확률
  dr = residuals(P339.fit, type = "deviance"), # 잔차(이탈도)
  pii = hatvalues(P339.fit),                  # 레버리지 값
  pr = residuals(P339.fit, type = "pearson"), # 피어슨 잔차
  cook = cooks.distance(P339.fit),           # Cook's distance
  dfbeta = dfbeta(P339.fit)  # DfBetas
)

par(mfrow=c(2,2))
plot(P339.fit)

library(car)
par(mfrow=c(1,1))
influenceIndexPlot(P339.fit)
influencePlot(P339.fit)

# Variable selection
P339.fit <- glm(Y~X1+X2+X3, family = binomial, data=P339) 
summary(P339.fit)
logLik(P339.fit)
summary(P339.fit)$null.deviance - summary(P339.fit)$deviance

P339.X12 <- glm(Y~X1+X2, family = binomial, data=P339) 
summary(P339.X12)
logLik(P339.X12)
summary(P339.X12)$null.deviance - summary(P339.X12)$deviance

P339.X1 <- glm(Y~X1, family = binomial, data=P339) 
summary(P339.X1)
logLik(P339.X1)
summary(P339.X1)$null.deviance - summary(P339.X1)$deviance

# Confusion matrix
predictions <- predict(P339.X12, type = "response") 
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
confusion_matrix <- table(P339$Y, predicted_classes)
print(confusion_matrix)

true_positives <- confusion_matrix[2, 2]
true_negatives <- confusion_matrix[1, 1]
false_positives <- confusion_matrix[1, 2]
false_negatives <- confusion_matrix[2, 1]

sensitivity <- true_positives / (true_positives + false_negatives)
specificity <- true_negatives / (true_negatives + false_positives)
accuracy <- (true_positives + true_negatives) / sum(confusion_matrix)
error_rate <- (false_positives + false_negatives) / sum(confusion_matrix)

cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Accuracy:", accuracy, "\n")
cat("Error Rate:", error_rate, "\n")

# C-index
n <- length(predictions)
concordant <- 0
discordant <- 0
ties <- 0

for (i in 1:(n - 1)) {
  for (j in (i + 1):n) {
    if (P339$Y[i] != P339$Y[j]) {
      if (predictions[i] == predictions[j]) {
        ties <- ties + 1
      } else if ((predictions[i] > predictions[j] && P339$Y[i] > P339$Y[j]) ||
                 (predictions[i] < predictions[j] && P339$Y[i] < P339$Y[j])) {
        concordant <- concordant + 1
      } else {
        discordant <- discordant + 1
      }
    }
  }
}

c_index <- concordant / (concordant + discordant + ties)
c_index
