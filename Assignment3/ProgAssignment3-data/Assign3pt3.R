outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[,11] <- as.numeric(outcome[,11])
tState <- table(outcome$State)
outcome2 <- subset(tState, 2 > 19)
table(outcome2$State)
tout <- subset(table(outcome, outcome$State >19))
