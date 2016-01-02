

#3
pnorm(-7.867)*2

#4
n1 <- 45+21
n2 <- 15+52

x1 <- 45/n1
x2 <- 15/n2

se <- sqrt((1-x1)/(n1*x1) + (1-x2)/(x2*n2))

#6
binom.test(3,4,alternative = "two.sided")