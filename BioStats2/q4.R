
#1
dat <- array(c(8, 52, 5, 164, 25, 29, 21, 128), c(2, 2, 2))
dat
mantelhaen.test(dat, correct = FALSE)

#2
diff <- c(-3, +2, -1) 
wilcox.test(diff, exact = T, alternative = c("two.sided"))

#3
#prop.test(15, 25, p = .5)

m <- matrix(c(55, 12, 41, 20), 2)
mcnemar.test(m, correct = FALSE) 

#4
a4 <- (55+41)*(41+20)/((55+12)*(12+20))

#5
exp(log(54/189))


#7
.8^5
