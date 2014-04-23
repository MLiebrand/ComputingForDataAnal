corr <- function(directory, threshold = 0) {
  ## ' directory is a character vector of length 1 indicating 
  ## the location of the CSV file
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations ( on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate: the default is 0
  
  ## return a numeric vector of correlations
  
  completeData <- complete(directory)
  dTotal <- length(completeData[,1])
  v <- vector(mode = "numeric", length = 0)
  for (i in 1:dTotal){
    nobs <- completeData$nobs[i]
    if (nobs > threshold) {
      v <- c(v, completeData$correlation[i])
    }
  }
  v
}