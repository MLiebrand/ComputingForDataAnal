complete <- function(directory, id = 1:332) {
  ## 'directory is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1   117
  ## 2   2041
  ##  ....
  ## where 'id is the monitor ID number and nobs is the
  ## number of completed cases
  
  max <- length(id)
  vg <- vector(mode = "numeric", length = 0)
  vid <- vector(mode = "numeric", length = 0)
  vcorns <- vector(mode = "numeric", length=0)
  for (i in 1:max){
    file <- fileName(directory, id[i])
    data2 <- read.csv(file)
    ccases <- complete.cases(data2)
    sulf <- data2[,2]
    sulf <- sulf[ccases]
    nitr <- data2[,3]
    nitr <- nitr[ccases]
    corns <- cor(nitr, sulf)
    if (is.na(corns)) {
      corns <- 0
    }
    vcorns <- c(vcorns, corns)
#    cData <- data2[ccases]
    g <- sum(ccases)
    vg <- c(vg, g)
    vid <- c(vid, id[i])
  }
#d <- data.frame(id=vid, nobs=vg)
d <- data.frame(id=vid, nobs=vg, correlation = vcorns)
}

fileName <- function (directory, id) {
  if (is.numeric(id)){
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
  paste(directory, "/", cId, ".csv", sep = "")
}