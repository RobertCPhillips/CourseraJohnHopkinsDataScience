x <- c(275,183,133,111,76,66,66,44,46)
xp <- x / sum(x)

xp.ben <- c(0.301,0.176,0.125,0.097,0.079,0.067,0.058,0.051,0.046)

z <- (xp-xp.ben)^2 / xp.ben
ts <- sum(z)
pchisq(ts, df=length(x)-1,lower.tail=F)


#dat <- c(275, 183, 133, 111, 76, 66, 66, 44, 56)
#p <- log10((1:9) + 1) - log10(1:9)
#chisq.test(dat, p = p, correct = FALSE)

#---------------------------------
x <- matrix(c(65,35,
              70,30,
              15,85),3,2,byrow=T)

x11 <- sum(x[,1])*sum(x[1,])/sum(x)
x21 <- sum(x[,1])*sum(x[2,])/sum(x)
x31 <- sum(x[,1])*sum(x[3,])/sum(x)
x12 <- sum(x[,2])*sum(x[1,])/sum(x)
x22 <- sum(x[,2])*sum(x[2,])/sum(x)
x32 <- sum(x[,2])*sum(x[3,])/sum(x)


#-----------------------------------------------