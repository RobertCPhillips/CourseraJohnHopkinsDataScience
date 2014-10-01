
q1 <- function() {
  x <- c(140,138,150,148,135)
  y <- c(132,135,151,146,130)
  t.test(x, y, paired=T,var.equal=T, alternative="two.sided")$p.value  
}

q2 <- function() {
  xbar <- 1100
  n <- 9
  se <- 30/sqrt(n)
  t <- qt(.975, df=n-1)
  
  xbar + c(-1,1)* t*se
}

q3 <- function() {
  x <- 3:4
  sum(dbinom(x,4 , .5))
}

q4 <- function() {
  
}

q5 <- function() {
  ybar <- -3
  xbar <- 1
  sy <- 1.5
  sx <- 1.8
  ny <- 9
  nx <- 9
  
  df <- (sy^2/ny+sx^2/nx)/(((sy/ny)^2/(ny-1)) + ((sx/nx)^2/(nx-1)))
  c1 <- ybar - xbar + c(-1,1)*qt(.975,df=df)*sqrt(sx^2/nx+sy^2/ny)
  c2 <- ybar - xbar + c(-1,1)*qt(.995,df=df)*sqrt(sx^2/nx+sy^2/ny)
  
  c(c1,c2)
}

q7 <- function() {
  n <- 100
  delta <- .01
  sigma <- .04
  alpha <- .05
  
  power.t.test(n=n,
               sd=sigma,
               delta=delta,
               sig.level=alpha,
               power=NULL,
               type="one.sample", alt="one.sided")$power
}

q8 <- function() {
  delta <- .01
  sigma <- .04
  alpha <- .05
  power <- .90
  power.t.test(sd=sigma,
               delta=delta,
               sig.level=alpha,
               power=power,
               type="one.sample", alt="one.sided")$n
}

q10 <- function() {
  m0 <- 42.04
  ma <- 44
  s <- 12
  n <- 288
  z <- (ma-m0)/s*sqrt(n)
  #pnorm(z)
  z
}