df <- read.table("studentmat.txt", header = TRUE)
head(df)

library(glmnet)

# Prepare the data
X <- as.matrix(df[, 1:26]) # Explanatory variables
y <- df$G3 # Response variable

# Fit the lasso model
fit_lasso <- cv.glmnet(X, y, family = "gaussian",
                       alpha = 1,                   # LASSO
                       nfolds = 10,
                       standardize = TRUE)

# Get the best lambda
best_lambda <- fit_lasso$lambda.min

# Fit the final model with the best lambda
final_model <- glmnet(X, y, family = "gaussian", 
                      alpha = 1, 
                      lambda = best_lambda,
                      standardize = TRUE)

# Get the coefficients
coef(final_model)