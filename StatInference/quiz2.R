## week 2 quiz 2

q3 <- function() {
  mu <- 1100
  sigma <- 75
  
  mu + sigma*1.645
}

q4 <- function() {
  mu <- 1100
  sigma <- 75
  n = 100
  
  mu + sigma*1.645/sqrt(100)
}

q3b <- function() {
  qnorm(.95, mean=1100, sd=75)
}

q5 <- function() {
  p <- .5
  n <- 5
  
  b <- function(x) {
    choose(n,x)* (p^x) * (1-p)^(n-x)
  }
  
  p4 <- b(4)
  p5 <- b(5)  
  p4 + p5
}

q5b <- function() {
  p <- .5
  n <- 5
  
  pbinom(3, n, p, lower.tail=FALSE)
}