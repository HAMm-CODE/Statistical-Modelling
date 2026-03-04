set.seed(260120262)

N  <- 100
x1 <- sort(runif(N, -5, 5))

# True coefficients
beta0 <- -0.5
beta1 <- 0.5

# Linear predictor
eta <- beta0 + beta1 * x1

# Mean via the inverse of log link
mu <- exp(eta)

# Draw from Poisson with mean mu
y <- rpois(N, lambda=mu)

plot(x1, y,
     pch = 16,
     col = "darkgreen",
     xlab = "x1",
     ylab = "y")


lines(x1, mu, col="darkgreen")

# Fit the Poisson regression model using glm-function
poisreg.fit <- glm(y ~ x1, family=poisson(link="log"))

# Summary-function gives detailed results
poisreg.res <- summary(poisreg.fit)
print(poisreg.res)

# Extract coefficient estimate for x1
beta1 <- coef(poisreg.res)["x1","Estimate"]

# 95%-confidence interval for the coefficients
ci95 <- confint(poisreg.fit)

# Incidence Rate Ratio (IRR)
OR_beta1 <- exp(beta1)
ci95_or_x1 <- exp(ci95["x1", ])

# You can get the covariance matrix of the maximum likelihood
# estimator using e.g. vcov function (?vcov)
Finv <- vcov(poisreg.fit)

# Let's calculate the approximate 95%-confidence interval for the 
# outcome mean
x1_new <- seq(-5, 5, length.out=100)

# R has many convenience functions e.g. for creating design/model matrices 
x_new <- model.matrix(~ x1_new)
print(x_new)

# Allocate vectors for confidence interval limits
mu_L <- rep(NA, nrow(x_new))
mu_U <- rep(NA, nrow(x_new))
mu_hat <- rep(NA, nrow(x_new))

for(i in 1:nrow(x_new)){
  # Select the i-th row of the design matrix
  xi <- x_new[i, ]
  
  # Linear predictor at xi
  eta_i <- sum(xi * coef(poisreg.fit))
  
  # Mean at xi
  mu_hat[i]  <- exp(eta_i)
  
  # Simple approximation for the link variance
  var_eta_i <- t(xi) %*% Finv %*% xi
  eta_L <- eta_i - 1.96 * sqrt(var_eta_i)
  eta_U <- eta_i + 1.96 * sqrt(var_eta_i)
  
  # Transform the limits to the mean scale
  mu_L[i]  <- exp(eta_L)
  mu_U[i]  <- exp(eta_U)
}

# True mean
plot(x1, mu,
     pch = 16,
     type = "l",
     col = "darkgreen",
     xlab = "x1",
     ylab = "y", lwd=3)

# Observations
points(x1, y,
       pch = 16,
       type = "p",
       col = "darkgreen",
       xlab = "x1",
       ylab = "y", lwd=3)

# Estimated mean
lines(x_new[,2], mu_hat, col="darkblue", lwd=3, lty=2)

# 95%-confidence interval for the mean
lines(x_new[,2], mu_L, col="darkblue", lty=3, lwd=3)
lines(x_new[,2], mu_U, col="darkblue", lty=3, lwd=3)

# Prediction interval

# For convenience, we use the MASS package
library(MASS)

y_pred_L <- rep(NA, nrow(x_new))
y_pred_U <- rep(NA, nrow(x_new))
y_pred_med <- rep(NA, nrow(x_new))

# Number of samples for the posterior predictive simulation
n_samples <- 10000

for(i in 1:nrow(x_new)){
  # Extract the i-th row of the design matrix
  xi <- x_new[i, ]
  
  # Draw samples from the approximate distribution of ML estimator
  beta_ast <- mvrnorm(n=n_samples, mu=coef(poisreg.fit), Sigma=Finv)
  
  # Linear predictor at xi
  eta_i <- xi %*% t(beta_ast)
  
  # Mean at xi
  mu_hat <- exp(eta_i)
  
  # Draw from Poisson with rates mu_hat
  ypred_sim <- rpois(n_samples, lambda=mu_hat)
  
  # Calculate the prediction limits
  y_pred_L[i]  <- quantile(ypred_sim, probs=0.025)
  y_pred_U[i]  <- quantile(ypred_sim, probs=0.975)
  y_pred_med[i]  <- median(ypred_sim)
}

# True observations
plot(x1, y,
     pch = 16,
     type = "p",
     col = "darkgreen",
     xlab = "x1",
     ylab = "y", lwd=3)

# Predicted observation
lines(x_new[,2], y_pred_med, col="darkblue", lwd=3, lty=2)

# 95%-confidence interval for the predicted observation
lines(x_new[,2], y_pred_L, col="darkblue", lty=3, lwd=3)
lines(x_new[,2], y_pred_U, col="darkblue", lty=3, lwd=3)



