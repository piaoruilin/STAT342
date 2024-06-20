#9.3#
#a
correlation_matrix <- cor(P256)
print(correlation_matrix)
pairs(P256)

#b
eigen_result <- eigen(correlation_matrix)
eigenvalues <- eigen_result$values
eigenvectors <- eigen_result$vectors
condition_number <- max(eigenvalues) / min(eigenvalues)

print(condition_number)

#c
threshold <- 0.1
small_eigen_indices <- which(eigenvalues < threshold)
cat("\nIndices of small eigenvalues (< 0.1):\n")
print(small_eigen_indices)

cat("\nEigenvectors corresponding to small eigenvalues:\n")
print(eigenvectors[, small_eigen_indices])

#d
model <- lm(y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11, data = P256)
vif_values <- vif(model)
cat("Variance Inflation Factors (VIF):\n")
print(vif_values)
cat("\nPredictors with VIF > 10 (indicating multicollinearity):\n")
print(vif_values[vif_values > 10])

#10.4#
#a
library(MASS)
scaled <- as.data.frame(scale(P290))
model <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = scaled)
summary(model)
summary_model <- summary(model)

#b
model2 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = P290)
summary(model2)
summary_model2 <- summary(model2)

#c
coeff <- summary_model$coefficients
coeff2 <- summary_model2$coefficients
print(coeff)
print(coeff2)

consistent <- all.equal(coeff, coeff2)
print(consistent)

#d
correlation_matrix <- cor(P290[, -1])
print(correlation_matrix)
pairs(P290[, -1])

#e
data_standardized <- scale(P290[, -1])
pca_result <- prcomp(data_standardized, scale. = TRUE)
print(pca_result$rotation)

sample_variances <- pca_result$sdev^2
print(sample_variances)

condition_number <- max(sample_variances) / min(sample_variances)
print(condition_number)

threshold <- 0.1
collinear_sets <- which(sample_variances < threshold)
print(length(collinear_sets))
print(collinear_sets)

#f
library(pls)
data_standardized <- as.data.frame(scale(P290[, -1]))
pcr_model <- pcr(Y ~ ., data = P290, scale = TRUE, validation = "CV")
summary(pcr_model)

validationplot(pcr_model, val.type = "MSEP")
n_components <- which.min(pcr_model$validation$PRESS)
print(n_components)

pcr_fit <- pcr(Y ~ ., data = P290, ncomp = n_components, scale = TRUE)
summary(pcr_fit)

#g
lambda_values <- seq(0, 100, by = 0.1)
ridge_model <- lm.ridge(Y ~ ., data = P290, lambda = lambda_values)

matplot(lambda_values, t(ridge_model$coef), type = "l")
legend("topright", legend = colnames(P290[, -1]), col = 1:ncol(P290[, -1]), lty = 1:ncol(P290[, -1]), cex = 0.8)

#h
data_standardized <- as.data.frame(scale(P290[, -1]))
data_standardized$Y <- scale(P290$Y)

lm_model <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = P290)
lm_standardized_model <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = data_standardized)

lambda_values <- seq(0, 100, by = 0.1)
ridge_model <- lm.ridge(Y ~ ., data = P290, lambda = lambda_values)

optimal_lambda <- ridge_model$lambda[which.min(ridge_model$GCV)]
print(optimal_lambda)

ridge_fit_optimal <- lm.ridge(Y ~ ., data = P290, lambda = optimal_lambda)

ols_coefficients <- coef(lm_model)
standardized_coefficients <- coef(lm_standardized_model)
ridge_coefficients <- ridge_fit_optimal$coef

print(ols_coefficients)
print(standardized_coefficients)
print(ridge_coefficients)

#11.5
#a
model <- lm(X9 ~ ., data = P329)
summary(model)

#b
model2 <- lm(X9 ~ X1 + X5 + X6, data = P329)
summary(model2)

#c
model_taxes <- lm(X9 ~ X1, data = P329)
model_excl_taxes <- lm(X9 ~ . - X1, data = P329)

summary(model_taxes)
summary(model_excl_taxes)
summary(model_all)

r_squared <- c(summary(model_taxes)$r.squared, summary(model_excl_taxes)$r.squared, summary(model_all)$r.squared)
adj_r_squared <- c(summary(model_taxes)$adj.r.squared, summary(model_excl_taxes)$adj.r.squared, summary(model_all)$adj.r.squared)

comparison <- data.frame(Model = c("Only Taxes", "Excluding Taxes", "All Variables"), R_Squared = r_squared, Adj_R_Squared = adj_r_squared)
print(comparison)

#11.6
#a
full_model <- lm(Y ~ ., data = P256)

vif_values <- vif(full_model)
vif_values

#b
stepwise_model <- stepAIC(full_model, direction = "both")
summary(stepwise_model)


#e
model1 <- lm(W ~ ., data = P256)
summary(model1)
