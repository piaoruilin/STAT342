##7.3##

install.packages(ggplot2)
install.packages("ggrepel")
install.packages(stats)
install.packages("base")

library(ggplot2)
library(ggrepel)
library(base)
library(stats)

OLS_model <- lm(Y~X1+X2+X3, data = P198)
summary(OLS_model)

#Leverage Values
pii <- round(as.vector(lm.influence(OLS_model)$hat),4)
pii

#Standardised Residuals
std_res <- rstandard(OLS_model)
std_res

#Cook's Distance
cook <- cooks.distance(OLS_model)
cook

#DFITS
dfits <- dffits(OLS_model)

#plotting
plot(pii, main = "Leverage Values", ylab = "Leverage", xlab = "Index")
plot(std_res, main = "Standardized Residuals", ylab = "Standardized Residual", xlab = "Index")
plot(cook, main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Index")
plot(dfits, main = "DFITS", ylab = "DFITS", xlab = "Index")

#identify
high_lev <- which(pii > 2 * mean(pii))
inf_cooks <- which(cook > 4 / length(cook))
inf_dfits <- which(abs(dfits) > 2 * sqrt(mean(pii)))

cat("High-leverage points:", high_lev, "\n")
cat("Influential points (Cook's distance):", inf_cooks, "\n")
cat("Influential points (DFITS):", inf_dfits, "\n")

##7.4##
P198$D1 <- ifelse(P198$Region == 1, 1, 0)
P198$D2 <- ifelse(P198$Region == 2, 1, 0)
P198$D3 <- ifelse(P198$Region == 3, 1, 0)
fit <- lm(Y ~ X1+X2+X3+D1+D2+D3, data = P198)
summary(fit)

reduced_fit <- lm(Y ~ X1 + X2 + X3, data = P198)
anova(reduced_fit, fit)

##8.4##
library(lmtest)

#P229$Date <- as.Date(P229$Date, format = "%m-%d-%Y")
#origin_date <- min(P229$Date)
#P229$Days <- as.numeric(difftime(P229$Date, origin_date, units = "Days"))
#head(P229)

data <- data.frame(
  Day = 1:261,
  DJIA = c(5117.12, 5177.45, 5194.07, 5173.84, 5181.43, 5197.68, 5130.13, 5032.94, 5065.10, 5061.12, 5043.78, 5088.22, 5066.90, 5124.35, 5184.68, 5219.36, 5192.27, 5242.84, 5216.83, 5271.75, 5304.98, 5381.21, 5395.30, 5405.06, 5373.99, 5407.59, 5459.61, 5492.12, 5539.45, 5541.62, 5600.15, 5601.23, 5579.55, 5551.37, 5503.32, 5503.32, 5458.53, 5515.97, 5608.46, 5630.49, 5565.10, 5549.21, 5506.21, 5485.62, 5536.56, 5600.15, 5642.42, 5629.77, 5641.69, 5470.45, 5581.00, 5583.89, 5568.72, 5586.06, 5584.97, 5683.60, 5669.51, 5655.42, 5626.88, 5636.64, 5643.86, 5670.60, 5626.88, 5630.85, 5587.14, 5637.72, 5671.68, 5689.74, 5682.88, 5682.88, 5594.37, 5560.41, 5485.98, 5487.07, 5532.59, 5592.92, 5620.02, 5549.93, 5551.74, 5535.48, 5564.74, 5588.59, 5553.90, 5566.91, 5567.99, 5573.41, 5569.08, 5575.22, 5498.27, 5478.03, 5464.31, 5420.95, 5474.06, 5475.14, 5518.14, 5582.60, 5624.71, 5625.44, 5635.05, 5687.50, 5748.82, 5736.26, 5778.00, 5762.12, 5762.86, 5762.86, 5709.67, 5673.83, 5693.41, 5643.18, 5624.71, 5665.71, 5697.48, 5667.19, 5697.11, 5687.87, 5668.66, 5668.29, 5657.95, 5649.45, 5652.78, 5628.03, 5648.35, 5659.43, 5705.23, 5717.79, 5719.27, 5682.70, 5677.53, 5654.63, 5729.98, 5720.38, 5703.02, 5703.02, 5588.14, 5550.83, 5581.86, 5603.65, 5520.50, 5510.56, 5349.51, 5358.76, 5376.88, 5464.18, 5426.82, 5390.94, 5346.55, 5354.69, 5422.01, 5473.06, 5434.59, 5481.93, 5528.91, 5594.75, 5679.83, 5674.28, 5696.11, 5718.67, 5713.49, 5681.31, 5704.98, 5647.28, 5666.88, 5665.78, 5689.45, 5699.44, 5721.26, 5689.82, 5733.47, 5722.74, 5693.89, 5711.27, 5712.38, 5647.65, 5616.21, 5616.21, 5648.39, 5656.90, 5606.96, 5659.86, 5733.84, 5727.18, 5754.92, 5771.94, 5838.52, 5889.20, 5888.83, 5877.36, 5867.74, 5888.46, 5894.74, 5874.03, 5877.36, 5868.85, 5872.92, 5882.17, 5904.90, 5933.97, 5932.85, 5992.86, 5979.81, 5966.77, 5930.62, 5921.67, 5969.38, 6010.00, 6004.78, 6020.81, 6059.20, 6094.23, 6090.87, 6061.80, 6036.46, 5992.48, 6007.02, 5972.73, 6007.02, 5993.23, 6029.38, 6021.93, 6041.68, 6081.18, 6206.04, 6219.82, 6255.60, 6266.04, 6274.24, 6313.00, 6348.03, 6346.91, 6397.60, 6430.02, 6418.47, 6471.76, 6547.79, 6528.41, 6499.34, 6499.34, 6521.70, 6521.70, 6442.69, 6422.94, 6437.10, 6381.94, 6463.94, 6473.25, 6402.52, 6303.71, 6304.87, 6268.35, 6308.33, 6346.77, 6473.64, 6484.40, 6489.02, 6522.85, 6522.85, 6546.68, 6560.91, 6549.37, 6448.27)
)

model <- lm(DJIA ~ Day, data = data)
summary(model)

par(mar = c(1,1,1,1))
plot(model)

hist(residuals(model), main = "Histogram of Residuals", xlab = "Residuals")

qqnorm(residuals(model))
qqline(residuals(model), col = "red")

plot(data$Day, residuals(model), type = "o", main = "Residuals Over Time", xlab = "Day", ylab = "Residuals")

#Durbin-Watson test
dw_test <- dwtest(model)
print(dw_test)

if (dw_test$p.value < 0.05) {
  cat("The p-value is", dw_test$p.value, ". There is significant evidence of autocorrelation in the residuals.\n")
} else {
  cat("The p-value is", dw_test$p.value, ". There is no significant evidence of autocorrelation in the residuals.\n")
}

##8.5##


