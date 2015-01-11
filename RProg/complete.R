complete <- function(directory, id = 1:332) {
  
  completeframe <- data.frame(numeric(0), numeric(0))
    
  for (i in id) {
    data <- read.csv(paste(directory, "/", getfilename(i), sep=""))
    completeframe <- rbind(completeframe, c(i, sum(complete.cases(data))))
  }
  
  names(completeframe) <- c("id", "nobs")
  completeframe
}

getfilename <- function(id) {
  if (id < 10)
    paste("00", id, ".csv", sep="")
  else if (id < 100)
    paste("0", id, ".csv", sep="")
  else 
    paste(id, ".csv", sep="")
}