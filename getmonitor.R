getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number.  The user can specify 'id' as either an integer, a 
  ## character or a numeric
  ## 'directory' is a character vector of lenght 1 indicating
  ## the location of the CSV files.
  
  ## 'summarize' is a logical indicating whether a summary of 
  ## the data should be printed to the console; the default is
  ## FALSE
  if (is.numeric(id) || is.integer(id)){
    if (id < 10) {
      cId <- paste("00", as.character(id), sep = "")
    }
    else if (id < 100) {
      cId <- paste("0", as.character(id), sep = "")
    }
    else
      cId <- as.character(id)
  }
  else {
    cId <- id
  }
  fileName <- paste(directory, "/", cId, ".csv", sep = "")
  
  print(cId)
  print(fileName)
  data3 <- read.csv(fileName)

  if (summarize==TRUE) {
    sumdat <- summary(data3)
   print(sumdat)
  }
##  else {
    data3
##  }
}

