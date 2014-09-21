# stat inf week 3 - quiz 3

q1 <- function() {
  n <- 9
  s <- 30
  xbar <- 1100
  xbar + c(-1,1)*qt(.975,n-1)*s/sqrt(n)
}

q2 <- function() {
  mn <- -2
  n <- 9
  -mn*sqrt(n)/qt(.975,n-1) #an s to get 0 in interval
}

q4 <- function() {
  nn <- 10; on <- 10
  nbar <- 3; obar <- 5
  nv <- .6; ov <- .68
  
  sp <- sqrt(((on-1)*ov + (nn-1)*nv)/(on+nn-2))
  i <- nbar - obar + c(-1,1)*qt(.975,nn+on-2)*sp*(1/on + 1/nn)^.5
  i
}

q6 <- function() {
  nn <- 100; on <- 100
  nbar <- 4; obar <- 6
  ns <- .5; os <- 2
  
  sp <- sqrt(((on-1)*os^2 + (nn-1)*ns^2)/(on+nn-2))
  i <- obar - nbar + c(-1,1)*qnorm(.975)*sp*(1/on + 1/nn)^.5
  i
}

q7 <- function() {
  tn <- 9; pn <- 9
  tbar <- -3; pbar <- 1;
  ts <- 1.5; ps <- 1.8
  
  sp <- sqrt(((tn-1)*ts^2 + (pn-1)*ps^2)/(tn+pn-2))
  i <- tbar - pbar + c(-1,1)*qt(.95,tn+pn-2)*sp*(1/tn + 1/pn)^.5
  i
}
