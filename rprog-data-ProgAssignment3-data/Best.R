best <- function(state, outcome) {
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    finalData <- data.frame()
    stateData <- outcomeData[outcomeData[,7]== state,]
    needColumn <- data.frame(idcol = c(11,17,23), name = c("heart attack","heart failure","pneumonia"))
    
    if (nrow(stateData) > 0) {
        if (any(m == outcome)) {
            numberCol <- needColumn$idcol[needColumn$name == outcome]
            stateData[,numberCol] <- as.numeric(stateData[,numberCol])
            finalData <- stateData[order(stateData[,numberCol],stateData[,2]),]
        }
        else {
            message("invalid outcome")
        }
    } 
    else {
       message("invalid state") 
    }
    finalData[1,2]
}