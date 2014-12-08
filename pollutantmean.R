pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  pollutantvector <- numeric(0)
    
  for (i in id) {
    data <- read.csv(paste(directory, "/", getfilename(i), sep=""))
    pollutantvector <- c(pollutantvector, data[[pollutant]])
  }
  
  mean(pollutantvector, na.rm=TRUE) 
}

getfilename <- function(id) {
  if (id < 10)
    paste("00", id, ".csv", sep="")
  else if (id < 100)
    paste("0", id, ".csv", sep="")
  else 
    paste(id, ".csv", sep="")
}