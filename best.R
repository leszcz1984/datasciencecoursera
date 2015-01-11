best <- function(state, outcome) {
  
  data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  correct_states = as.character(unique(data[["State"]]))
  correct_outcomes = list("heart attack" = 11, "heart failure"= 17, "pneumonia"=  23)
  
  if (!(state %in% correct_states))
    stop("invalid state")
  if (!(outcome %in% names(correct_outcomes)))
    stop("invalid outcome")
  
  outcome_id = as.numeric(correct_outcomes[outcome])  
    
  hospitals_with_data <- subset(data, data[["State"]] == state & data[[outcome_id]] != "Not Available", select=c(2,outcome_id))    
  hospitals_with_data[,2] <- as.numeric(hospitals_with_data[,2])
  hospitals_with_lowest_outcome <- subset(hospitals_with_data, hospitals_with_data[,2] == min(hospitals_with_data[,2]), select=c(1))
  
  sort(hospitals_with_lowest_outcome[[1]])[1]
  
}

