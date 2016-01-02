#--------------------------------------------------------------------------------
#1 - one sided test
#--------------------------------------------------------------------------------
n <- 100
x <- 12
s <- 4
a <- .05
za <- qnorm(1-a)

se <- s/sqrt(n)

rr <- x - za*se

#--------------------------------------------------------------------------------
#2 - paired and unpaired
#--------------------------------------------------------------------------------
n <- 5
x1 <- c(140,138,150,148,135)
x2 <- c(138,136,148,146,133)

#paired
d <- x2-x1

d_bar <- mean(d)
d_sd <- sd(d)

se <- d_sd/sqrt(n)
#ts <- d_bar/se # se=0, implies inf?

#unpaired
n1 <- n2 <- n
x1b <- mean(x1)
s1 <- sd(x1)

x2b <- mean(x2)
s2 <- sd(x2)

d <- x1b - x2b

sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2))
se <- sp*sqrt(1/n1 + 1/n2)

t <- d/se
ta <- qt(.025,df=n1+n2-2)
r <- t < ta

#--------------------------------------------------------------------------------
#4 - one sided t test, paired sample, test that new has lower times
#5 - 95% ci for ratio of waiting times
#--------------------------------------------------------------------------------
n <- 5
#log hours
new <- c(0.929, -1.745, 1.677, 0.701, 0.128)
old <- c(2.233, -2.513, 1.204, 1.938, 2.533)

d <- new - old

d_bar <- mean(d)
d_sd <- sd(d)

se <- d_sd/sqrt(n)
t <- d_bar/se
p <- pt(t, df=n-1, lower.tail=T)

new2 <- exp(new)
old2 <- exp(old)
ratio <- new2/old2
ratio.ln <- log(ratio)

ci <- mean(ratio.ln) + c(-1,1)*qt(.95,df=n-1)*sd(ratio.ln)/sqrt(n)
ci.a <- exp(ci)
#--------------------------------------------------------------------------------
#6 - 2 groups independent, 2 sided test, followup - baseline
#--------------------------------------------------------------------------------
n1 <- n2 <- 9

xdt <- -3
xdp <- 1

st <- 1.5
sp <- 1.8

sp <- sqrt(((n1 - 1) * st^2 + (n2 - 1) * sp^2)/(n1 + n2 - 2))
se <- sp*sqrt(1/n1 + 1/n2)

t <- (xdt - xdp)/se
p <- pt(t,df=n1+n2-2) * 2
