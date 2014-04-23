rAllHospital <- function(outcomeData, measure) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death rate
  
outcomeData <-read.csv("outcome-of-care-measures.csv", colClasses ="character")
outcomes <- c("heart attack", "heart failure", "pneumonia")
 
if (any(outcome==outcomes, na.rm=TRUE)==FALSE){
  stop("invalid outcome")
}
stOutcomeData <- subset(outcomeData, outcomeData$State==state)

if (outcome == outcomes[1]){
  stOutcomeData[,11] <- as.numeric(stOutcomeData[,11])
  subData2 <- stOutcomeData[order((stOutcomeData[,11]), (stOutcomeData[,2]), na.last = NA), ]
  
}
if (outcome == outcomes[2]){
  stOutcomeData[,17] <- as.numeric(stOutcomeData[,17])
  subData2 <- stOutcomeData[order((stOutcomeData[,17]), (stOutcomeData[,2]), na.last = NA), ]
}
if (outcome == outcomes[3]){
  stOutcomeData[,23] <- as.numeric(stOutcomeData[,23])
  subData2 <- stOutcomeData[order((stOutcomeData[,23]), (stOutcomeData[,2]), na.last = NA), ]

num <- as.numeric(num)
 

subData2
}