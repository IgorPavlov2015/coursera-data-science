pollutantmean <- function(directory,pollutant,id = 1:332){
    fileResult <- 0
    meanResult <- c()
    fileCsv <- ""
    for (st in id) {
        fileName <- ""
        strCharacter <- as.character(st)
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
        filePollutant <- fileCsv[pollutant]
        badResult <- is.na(filePollutant)
        goodResult <- filePollutant[!badResult]
        meanResult <- rbind(c(meanResult,goodResult))
    }
    
    mean(meanResult)
} 
    
