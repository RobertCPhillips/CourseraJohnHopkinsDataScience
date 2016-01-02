
#1
x1 <- matrix(c(4,1,2,6),2)
fisher.test(x1)

#2
x2 <- matrix(c(3,2,1,4),2)
x2.a <- fisher.test(x2,alternative = "greater")$p.value
x2.b <- choose(4,3)*choose(6,2)/choose(10,5) + choose(4,4)*choose(6,1)/choose(10,5)

#3
x3 <- c(140,100,50)
x3.o <- x3 / sum(x3)
x3.e <- c(.53, .35, .12)

x3.a <- chisq.test(x3, p=x3.e, correct=F)

x3.ts <- sum((x3.o-x3.e)^2 / x3.e)
x3.b <- pchisq(x3.ts, df=2,lower.tail=F)

#4
x4 <- matrix(c(80,15,5,
               60,30,10),2,3,byrow=T)

x4.11 <- sum(x4[,1])*sum(x4[1,])/sum(x4)
x4.21 <- sum(x4[,1])*sum(x4[2,])/sum(x4)
x4.12 <- sum(x4[,2])*sum(x4[1,])/sum(x4)
x4.22 <- sum(x4[,2])*sum(x4[2,])/sum(x4)
x4.13 <- sum(x4[,3])*sum(x4[1,])/sum(x4)
x4.23 <- sum(x4[,3])*sum(x4[2,])/sum(x4)


#5
x5 <- matrix(c(43, 4,
                8,45),2,2,byrow=T)
x5.a <- chisq.test(x5)


#7
x7 <- c(46,54,49,51)
x7.e <- c(.25,.25,.25,.25)
x7.a <- chisq.test(x7, p = x7.e, correct = F)

#8
x8 <- matrix(c(65,70,15,
               35,30,85),3,2)

x8.a <- chisq.test(x8, correct=F)
