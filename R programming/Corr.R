corr <- function(directory, threshold = 0) {
    numberObjects <- length(dir(directory))
    id <- 1:numberObjects
    corelation <- vector("numeric")
    for (i in id) {
        fileName <- ""
        strCharacter <- as.character(i)
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
        completeFile <- complete.cases(fileCsv)
        numberComplete <- sum(completeFile)
        if (numberComplete > threshold) {
            readyFile <- fileCsv[completeFile,]
            corelation <- append(corelation,cor(readyFile$nitrate,readyFile$sulfate))
        }
    }
    corelation
}