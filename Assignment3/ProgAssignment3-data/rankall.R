rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that outcome is valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abreviated) state name
  
  outcomeData <-read.csv("outcome-of-care-measures.csv", colClasses ="character")
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  vStates <- unique(outcomeData$State)
  stateHold <- "XX"
  sortdf <- NULL
  vHospitals <- vector(mode = "character", length=0)
  if (any(outcome==outcomes, na.rm=TRUE)==FALSE){
    stop("invalid outcome")
  }
  if (outcome == outcomes[1]){
    outcomeData[,11] <- as.numeric(outcomeData[,11])
    subData2 <- outcomeData[order((outcomeData[,7]), (outcomeData[,11]), (outcomeData[,2]), na.last = NA), ]
    sortdf <- (cbind((subData2[,7]), (subData2[,11]), (subData2[,2])))    
  }
  if (outcome == outcomes[2]){
    outcomeData[,17] <- as.numeric(outcomeData[,17])
    subData2 <- outcomeData[order((outcomeData[,7]), (outcomeData[,17]), (outcomeData[,2]),  na.last = NA), ]
    sortdf <- (cbind((subData2[,7]), (subData2[,17]), (subData2[,2])))    
  }
  if (outcome == outcomes[3]){
    outcomeData[,23] <- as.numeric(outcomeData[,23])
    subData2 <- outcomeData[order((outcomeData[,7]), (outcomeData[,23]), (outcomeData[,2]),  na.last = NA), ]
    sortdf <- (cbind((subData2[,7]), (subData2[,23]), (subData2[,2])))    
  }
  if (num=="best"){
    num <- 1
  }  
  if (num=="worst"){
    num <- 0
  }  
  num <- as.integer(num)
  stateRank <- 1
  stRank <- vector(mode = "numeric", length(subData2$State))
  stWorst <- vector(mode = "character", length(subData2$State))  
  for (i in 1:length(sortdf[,1])){
    if (sortdf[i,1] != stateHold) {
      stateRank <- 1
      if (i > 1) {
        stWorst[i-1] = "W" 
      }
    }
    stRank[i] <- stateRank
    stateRank <- stateRank+1
    stateHold <- sortdf[i,1]
  }
  stWorst[i] = "W"
  sortdf <- cbind(sortdf,stRank, stWorst)
  if (num==0){
    sortdf <- subset(sortdf, sortdf[,5]=="W")
  } else {
    sortdf <- subset(sortdf, sortdf[,4]==num)
  }
  stateDiff <- setdiff(vStates,  sortdf[,1])
  naVector <- rep(NA_character_, length(stateDiff))
  returndf <- data.frame(hospital = c(sortdf[,3], naVector) , state=c(sortdf[,1], stateDiff))
  returndf <- returndf[order(returndf[,2]),]
  returndf
}