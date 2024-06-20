import numpy as np
import pandas as pd
from sklearn.datasets import make_regression
from sklearn.linear_model import LinearRegression
from statsmodels.api import OLS, add_constant

# Generate example data
X, y = make_regression(n_samples=100, n_features=10, noise=0.1, random_state=42)
data = pd.DataFrame(X, columns=[f'x{i+1}' for i in range(X.shape[1])])
data['y'] = y

# Helper function to calculate p-values
def calculate_pvalues(X, y):
    model = OLS(y, add_constant(X)).fit()
    return model.pvalues

# Backward Elimination
def backward_elimination(data, target, significance_level=0.05):
    predictors = data.columns.tolist()
    predictors.remove(target)
    while True:
        pvalues = calculate_pvalues(data[predictors], data[target])
        max_pvalue = pvalues.max()
        if max_pvalue > significance_level:
            excluded_predictor = pvalues.idxmax()
            predictors.remove(excluded_predictor)
        else:
            break
    return predictors

# Forward Selection
def forward_selection(data, target, significance_level=0.05):
    initial_predictors = []
    remaining_predictors = data.columns.tolist()
    remaining_predictors.remove(target)
    while remaining_predictors:
        pvalues = pd.Series(index=remaining_predictors)
        for predictor in remaining_predictors:
            model = OLS(data[target], add_constant(data[initial_predictors + [predictor]])).fit()
            pvalues[predictor] = model.pvalues[predictor]
        min_pvalue = pvalues.min()
        if min_pvalue < significance_level:
            best_predictor = pvalues.idxmin()
            initial_predictors.append(best_predictor)
            remaining_predictors.remove(best_predictor)
        else:
            break
    return initial_predictors

# Perform Backward Elimination
predictors_BE = backward_elimination(data, 'y')
p_BE = len(predictors_BE)
print(f"Backward Elimination retained predictors: {predictors_BE}")
print(f"Number of predictors in final model (p_BE): {p_BE}")

# Perform Forward Selection
predictors_FS = forward_selection(data, 'y')
p_FS = len(predictors_FS)
print(f"Forward Selection retained predictors: {predictors_FS}")
print(f"Number of predictors in final model (p_FS): {p_FS}")