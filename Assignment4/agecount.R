agecount <- function(age = NULL) {
  ## Check that "are" is non-Null; else throw error
  ## Check that specific "cause" is allowed; else throw error
  ## Read "homicides.txt" data file
  ## Extract ages of victims; ignor records where no age is given
  ## Return integer containing count of homicides for that age

 
  if (is.null(age)) {
    stop("age is null")
  }
  homicides <- readLines("homicides.txt")           
  regexp <- paste(" ", as.character(age), " years old", sep="")
  tot <- grep(regexp, homicides)
  length(tot)
}