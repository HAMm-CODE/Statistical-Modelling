set.seed(10022062)

N <- 240
# Period (hours in day)
Tper <- 24                      
t <- sort(runif(N, 0, 72))      # 3 days of irregular sampling
epsilon <- rnorm(N, sd = 1.0)
beta0 <- 10
beta1 <- 3
beta2 <- -2
ymean <- beta0 +
  beta1 * cos(2 * pi * (1 / Tper) * t) +   # k=1 cosine
  beta2 * sin(2 * pi * (2 / Tper) * t)   # k=2 sine (period 12)
  
y <- ymean +  eps

fourier_design <- function(t, K, Tper) {
  X <- cbind(Intercept = rep(1, length(t)))
  for (k in 1:K) {
      X <- cbind(X, cos(2*pi*k*t/Tper), sin(2*pi*k*t/Tper))  
      colnames(X)[ncol(X)-1] <- paste0("cos_", k)
      colnames(X)[ncol(X)]   <- paste0("sin_", k)
      
    }
  return(X)
}

# Fit final model
X <- fourier_design(t, 5, Tper)

matplot(t, X, type="l", lwd=2, col=1:ncol(X), ylab="Basis functions", main="Fourier basis")

# -1 to remove intercept added by lm
fourier_fit <- lm(y ~ X - 1)  
summary(fourier_fit)


newdata <- data.frame(t=seq(0, 72, length.out=N))
pred <- predict(fourier_fit, newdata=newdata, se.fit=TRUE, interval="prediction")

plot(newdata$t, pred$fit[,1], type="l", col="blue", lwd=2,
     xlab="time", ylab="Predicted mean", main="Fourier regression fit")
lines(newdata$t, pred$fit[,2], lty=2, col="blue", lwd=1)
lines(newdata$t, pred$fit[,3], lty=2, col="blue", lwd=1)
points(t, y, pch=16, col="black")

