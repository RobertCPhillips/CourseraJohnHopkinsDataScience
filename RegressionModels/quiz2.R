#regression models week 2 quiz 2

q1 <- function() {
  x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
  y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
  n <- length(y)
  beta1 = cor(y,x) * sd(y) / sd(x)
  beta0 = mean(y) - beta1*mean(x)
  yhat <- beta0 + beta1*x
  e <- y - yhat
  varhat <- sum(e^2)/(n-2)
  sigmahat <- sqrt(varhat)
  ssx <- sum((x-mean(x))^2)
  seBeta1 = sigmahat / sqrt(ssx)
  seBeta0 <- sqrt((1/n) + (mean(x)^2)/ssx) * sigmahat
  tBeta0 <- beta0/seBeta0
  tBeta1 <- beta1/seBeta1
  pBeta0 <- 2*pt(abs(tBeta0), df=n-2, lower.tail=FALSE)
  pBeta1 <- 2*pt(abs(tBeta1), df=n-2, lower.tail=FALSE)
  
  coefTable <- rbind(c(beta0,seBeta0,tBeta0,pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
  colnames(coefTable) <- c("Estimate", "Std Error", "t value", "P(>|t|)")
  rownames(coefTable) <- c("(Intercept)", "x")
  
  coefTable
}

q2 <- function() {
  x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
  y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
  n <- length(y)
  beta1 = cor(y,x) * sd(y) / sd(x)
  beta0 = mean(y) - beta1*mean(x)
  yhat <- beta0 + beta1*x
  e <- y - yhat
  varhat <- sum(e^2)/(n-2)
  sigmahat <- sqrt(varhat)
  
  sigmahat
}

q3 <- function(carData) {
  x <- carData$wt
  y <- carData$mpg 
  n <- length(y)
  
  beta1 = cor(y,x) * sd(y) / sd(x)
  beta0 = mean(y) - beta1*mean(x)
  
  yhat <- beta0 + beta1*x
  e <- y - yhat
  varhat <- sum(e^2)/(n-2)
  sigmahat <- sqrt(varhat)
  ssx <- sum((x-mean(x))^2)
  
  se1 <- sigmahat * sqrt(1/n)
  beta0 + beta1*mean(x) - qt(.975, df=n-2)*se1
}

q3b <- function(carData) {
  x <- carData$wt
  y <- carData$mpg 
  fit <- lm(y~x)
  newData <- data.frame(x=mean(x))
  
  p1 <- predict(fit, newData, interval="confidence")
  p1
}

q5 <- function(carData, xVal) {
  x <- carData$wt
  y <- carData$mpg 
  n <- length(y)
  
  beta1 = cor(y,x) * sd(y) / sd(x)
  beta0 = mean(y) - beta1*mean(x)
  
  yhat <- beta0 + beta1*x
  e <- y - yhat
  varhat <- sum(e^2)/(n-2)
  sigmahat <- sqrt(varhat)
  ssx <- sum((x-mean(x))^2)
  
  se2 <- sigmahat * sqrt(1 + 1/n + (xVal - mean(x))^2/ssx)
  beta0 + beta1*xVal + qt(.975, df=n-2)*se2
}

q5b <- function(carData, xVal) {
  x <- carData$wt
  y <- carData$mpg 
  
  fit <- lm(y~x)
  newData <- data.frame(x=xVal)
  
  p2 <- predict(fit, newData, interval="prediction")
  p2
}

q6 <- function(carData) {
  x <- carData$wt
  y <- carData$mpg 
  n <- length(y)
  
  beta1 = cor(y,x) * sd(y) / sd(x)
  beta0 = mean(y) - beta1*mean(x)
  
  yhat <- beta0 + beta1*x
  e <- y - yhat
  varhat <- sum(e^2)/(n-2)
  sigmahat <- sqrt(varhat)
  ssx <- sum((x-mean(x))^2)
  
  se1 <- sigmahat / sqrt(ssx)
  2*(beta1 - qt(.975, df=n-2)*se1)  
}

q9 <- function(carData) {
  x <- carData$wt
  y <- carData$mpg 
  n <- length(y)
  
  beta1 = cor(y,x) * sd(y) / sd(x)
  beta0 = mean(y) - beta1*mean(x)
  
  yhat <- beta0 + beta1*x
  e <- y - yhat
  sum(e^2)/sum((y-mean(y))^2)
}