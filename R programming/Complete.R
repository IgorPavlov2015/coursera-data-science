complete <- function(directory, id = 1:332) {
    fileCsv <- c()
    cases <- c()
    dataFrame <- data.frame()
    for (n in id) {
        fileName <- ""
        strCharacter <- as.character(n)
        lenghtFile <- nchar(strCharacter)
        if(lenghtFile<3){
            zero <- ""
            if(lenghtFile<2){
                zero <- "00"
            } else{
                zero <- "0"
            }
            fileName <- paste(c(zero,strCharacter),collapse = "")
        } else {
            fileName <- strCharacter
        }
        fileCsv<-read.csv(paste(c(directory,fileName,".csv"),collapse = ""))
        cases <- complete.cases(fileCsv)
        nobs <- sum(cases)
        fileId <- n
        new_row <- c(fileId,nobs)
        dataFrame <- rbind(dataFrame,new_row)
        names(dataFrame) <- c("id","nobs")
    }
    dataFrame
}