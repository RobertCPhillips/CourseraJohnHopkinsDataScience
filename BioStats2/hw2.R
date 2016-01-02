#2  - probably of a win
odds <- 4/1

p <- 1 / (odds + 1)


#3 - stress levels from two occupations, 
#   the following data were obtained where there were 100 in each group 

n1 <- n2 <- 100
x1 <- 70 #(n1-70)
x2 <- 15 #(n2-15)
ph <- (x1+x2)/(n1+n2)

ts <- (x1/n1-x2/n2)/sqrt(ph*(1-ph)*(1/n1+1/n2))
#ci <- (x1-x2) + c(-1,1)*z*ts

#4 - 
se <- sqrt(1/45+1/21+1/15+1/52)
n2 <- (15+52)
