rankhospital <- function(state, outcome, num = "best") { 

  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (is.null(state)) 
    stop("invalid state")
  
  if (!state %in% outcomes[,7]) 
    stop("invalid state")
  
  if (is.null(outcome)) 
    stop("invalid outcome")
  
  validOutcomes = data.frame(c("heart attack", "heart failure", "pneumonia"), 
                             c(11, 17, 23))
  
  if (!outcome %in% validOutcomes[,1]) 
    stop("invalid outcome")
  
  if (is.null(num))
    stop("invalid num")
  
  r <- as.numeric(num)
  if (num != "best" && num != "worst" && is.na(r)) 
    stop("invalid num")
  
  ## Return hospital name in that state with the given rank 
  ## 30-day death rate

  idx = validOutcomes[validOutcomes == outcome, 2]
  
  outcomes[, idx] <- as.numeric(outcomes[, idx])
  ordered <- outcomes[outcomes[,7] == state & !is.na(outcomes[,idx]),c(2,idx)]
  ordered <- ordered[order(ordered[,2], ordered[,1]),]
  
  l <- length(ordered[,1])
  
  if (num == "best") ordered[1,1]
  else if (num == "worst") ordered[l, 1]
  else if (num > l) NA
  else ordered[num,1]  
}
