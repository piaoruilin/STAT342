#5.2
plot(P159)
model <- lm(P159)
summary(model)

f_statistic <- summary(model)$fstatistic[1]
p_value <- summary(model)$fstatistic[2]

coef_index <- 2
t_value <- summary(model)$coefficients[coef_index, "t value"]
p_value <- summary(model)$coefficients[coef_index, "Pr(>|t|)"]

full_model <- lm(Age ~ Height + Weight, data = P159)
reduced_model <- lm(Age ~ 1, data = P159)

f_test <- anova(reduced_model, full_model)
f_test

summary(full_model)

#5.5
plot(P155a)

model <- lm(Y ~ Gender, data = P155a)
residuals <- residuals(model)
var_Y <- var(residuals + fitted(model))

summary_model <- summary(model)
r_squared <- summary_model$r.squared
r_squared

confidence_intervals <- confint(model, level = 0.95)
confidence_intervals

#5.9
model1 <- lm(V ~ I+D+W+G*I+P+N, data=P160)
coef(model1)

fullmodel <- lm(V ~ ., data=P160[-1])
summary(fullmodel)

#6.3
model2 <- lm(R~P, data=P187)
summary(model2)

stanres <- rstandard(model2)
plot(P187$P, stanres,xlab="P",ylab="Standardizes Residual",main="graph")

#6.8
install.packages("ggplot2")
library(ggplot2)

t <- c(1,2,3,4,5,6,7,8,9,10,11)
model3 <- lm(Price~t, data=P190)
summary(model3)

ggplot(P190, aes(x=t, y=Price)) + geom_point() + labs(x="t", y="price", title="Price vs t")