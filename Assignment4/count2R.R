count <- function(cause = NULL) {
  ## Check that "cause" is non-Null; else throw error
  ## Check that specific "cause" is allowed; else throw error
  ## Read "homicides.txt" data file
  ## Extract causes of death
  ## Return integer containing count of homicides for that cause

  causes <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")  
  if (any(cause == causes, na.rm=TRUE)==FALSE) {
    stop("invalid cause")
  }
  homicides <- readLines("homicides.txt")
  firstChar <- paste("[", toupper(substring(cause,1,1)), tolower(substring(cause,1,1)),"]", sep="")
  thereset <- paste("[", substring(cause,2,), "]", sep="")              
  regexp <- paste("Cause: ", firstChar, thereset, sep="")
  tot <- grep(regexp, homicides)
  length(tot)
}