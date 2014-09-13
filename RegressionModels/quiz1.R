#regression models week 1 quiz 1

q1 <- function() {
  x <- c(0.18, -1.54, 0.42, 0.95)
  w <- c(2, 1, 3, 1)
  mm <- matrix(c(.0025, .1471, .3, 1.077))
  
  xm <- matrix(x)
  
  result <- apply(mm, 1, function(mu) {w %*% ((xm - mu)^2)})
  
  mm[which(result == min(result))]
}

q2 <- function() {
  x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
  y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
  lm(y~x-1)
}

q3 <- function() {
  data(mtcars)
  lm(mtcars$mpg~mtcars$wt)
}

q6 <- function() {
  x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
  
  m <- mean(x)
  n = length(x)
  
  (x - m)/sd(x)
}

q7 <- function() {
  x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
  y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
  lm(y~x)
}

q9 <- function() {
  x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
  lm(x~1)
}