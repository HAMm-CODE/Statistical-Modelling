#loaded data from the cs file with my column as V82
data <- read.csv("./fourier_regression_data.csv")
summary(data)

t <- data[,1]
y <- data[,2]

fourier_terms <- function(t, K, T=1) {
  mat <- matrix(nrow=length(t), ncol=2*K)
  for (k in 1:K) {
    mat[, 2*k-1] <- sin(2*pi*k*t / T)
    mat[, 2*k]   <- cos(2*pi*k*t / T)
  }
  as.data.frame(mat)
}

aic_values <- numeric(5)
models <- list()

for (K in 1:5) {
  X <- fourier_terms(t, K)
  df <- cbind(y=y, X)
  fit <- lm(y ~ ., data=df)
  models[[K]] <- fit
  aic_values[K] <- AIC(fit)
  cat("K =", K, "| AIC =", AIC(fit), "\n")
}

best_K <- which.min(aic_values)
cat("Best K:", best_K, "\n")

# Plot
plot(t, y, main="Fourier Regression Fit", pch=16, col="gray")
t_seq <- seq(min(t), max(t), length.out=500)
X_pred <- fourier_terms(t_seq, best_K)
y_pred <- predict(models[[best_K]], newdata=X_pred)
lines(t_seq, y_pred, col="red", lwd=2)

plot(newdata$t, pred$fit[,1], type="l", col="blue", lwd=2,
     xlab="time", ylab="Predicted mean", main="Fourier regression fit")
lines(newdata$t, pred$fit[,2], lty=2, col="blue", lwd=1)
lines(newdata$t, pred$fit[,3], lty=2, col="blue", lwd=1)
points(t, y, pch=16, col="black")
