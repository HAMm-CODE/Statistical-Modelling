set.seed(260120262)

N  <- 100
x1 <- sort(runif(N, -5, 5))

# True coefficients
beta0 <- -0.5
beta1 <- 0.5

# Linear predictor mean
eta <- beta0 + beta1 * x1

mu <- eta

# Draw from normal with mean mu and sd = 1
y <- rnorm(N, mean=mu, sd=1)

plot(x1, y,
     pch = 16,
     col = "darkgreen",
     xlab = "x1",
     ylab = "y")


lines(x1, mu, col="darkgreen")

normalreg.fit <- glm(y ~ x1, family=gaussian(link="identity"))
normalreg.res <- summary(normalreg.fit)
print(normalreg.res)
beta1 <- coef(normalreg.res)["x1","Estimate"]

ci95 <- confint(normalreg.fit)

ci95_x1 <- ci95["x1", ]

# You can get the covariance matrix of the maximum likelihood
# estimator using e.g. vcov function (?vcov)
Finv <- vcov(normalreg.fit)

# Let's calculate the approximate 95%-confidence interval for the 
# outcome mean
x1_new <- seq(-5, 5, length.out=100)

# R has many convenience functions e.g. for creating design/model matrices 
x_new <- model.matrix(~ x1_new)

print(x_new)
mu_L <- rep(NA, nrow(x_new))
mu_U <- rep(NA, nrow(x_new))
mu_hat <- rep(NA, nrow(x_new))

for(i in 1:nrow(x_new)){
  xi <- x_new[i, ]
  eta_i <- sum(xi * coef(normalreg.fit))
  mu_hat[i]  <- eta_i
  
  # Simple approximation for the link variance
  var_eta_i <- t(xi) %*% Finv %*% xi
  eta_L <- eta_i - 1.96 * sqrt(var_eta_i)
  eta_U <- eta_i + 1.96 * sqrt(var_eta_i)
  
  mu_L[i]  <- eta_L
  mu_U[i]  <- eta_U
}

# True mean
plot(x1, mu,
     pch = 16,
     type = "l",
     col = "darkgreen",
     xlab = "x1",
     ylab = "y", lwd=3)

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

n_samples <- 10000

for(i in 1:nrow(x_new)){
  xi <- x_new[i, ]
  beta_ast <- mvrnorm(n=n_samples, mu=coef(normalreg.fit), Sigma=Finv)
  eta_i <- xi %*% t(beta_ast)
  
  mu_hat <- eta_i
  
  # Draw from normal with rates mu_hat and sd sigma_hat
  sigma_hat <- sqrt(normalreg.res$dispersion)
  ypred_sim <- rnorm(n_samples, mean=eta_i, sd=sigma_hat)
  
  # But we could also pick quantiles directly from mu_hat
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



