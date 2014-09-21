#show that average converges tothe expected value of 1/lambda
f1 <- function(lambda, nosim, sample) {
  #means <- cumsum(rexp(sample, lambda)) / (1  : nosim)
  means <- sapply(1:nosim, function(i) mean(rexp(sample,lambda)))
  
  library(ggplot2)
  
  g <- ggplot(data.frame(x = 1 : nosim, y = means), aes(x = x, y = y)) 
  g <- g + geom_hline(yintercept = 0) + geom_line(size = 2) 
  g <- g + geom_hline(yintercept = 1/lambda, size = 4,colour="red") 
  g <- g + labs(x = "Number of simulations", y = "Sample means")
  g
}