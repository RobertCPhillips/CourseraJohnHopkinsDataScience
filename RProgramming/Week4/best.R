best <- function(state, outcome) { 
  
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (is.null(state)) 
    stop("invalid state")
  
  if (!state %in% outcomes[,7]) 
    stop("invalid state")
  
  if (is.null(outcome)) 
    stop("invalid outcome")
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) 
    stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death rate
  ## 11 - "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  ## 17 - "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  ## 23 - "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  
  idx = 0
  if (outcome == "heart attack") idx = 11
  else if (outcome == "heart failure") idx = 17
  else idx = 23
  
  outcomes[, idx] <- as.numeric(outcomes[, idx])
  ordered <- outcomes[outcomes[,7] == state & !is.na(outcomes[,idx]),c(2,idx)]
  ordered[order(ordered[,2], ordered[,1]),][1,1]
}
