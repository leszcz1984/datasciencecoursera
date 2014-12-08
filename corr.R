corr <- function(directory, threshold = 0) {
  
  correlations <- numeric(0)
    
  for (file in list.files(directory)) {    
    data <- read.csv(paste(directory, "/", file, sep=""))
    if (sum(complete.cases(data)) <= threshold)
      next
    correlations <- c(correlations, cor(data[["sulfate"]], data[["nitrate"]], use="complete.obs"))
  }
    
  correlations
}

