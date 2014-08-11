complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        files <- dir(directory, full.names=TRUE)[id]      
        fileCount <- length(files)
        
        allData <- data.frame(Id=rep(0,fileCount), nobs=0)        
        
        for (f in 1:fileCount) {
                fileData <- read.csv(files[f])
                id <- fileData[1,4]
                index <- complete.cases(fileData)
                completeDataCount <- sum(index)
                allData[f,] <- c(id, completeDataCount)
        }
        
        allData
}