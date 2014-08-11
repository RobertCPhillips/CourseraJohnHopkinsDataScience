corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        monitorsToProcess <- complete(directory)
        monitorIdsToProcess <- monitorsToProcess[monitorsToProcess["nobs"] > threshold,1]
        
        result <- c()
        
        for(id in monitorIdsToProcess) {
                monitorFile <- sprintf("%s/%03d.csv", directory, id)
                monitorData <- read.csv(monitorFile)
                correlation <- cor(monitorData["sulfate"], monitorData["nitrate"], use="complete")
                result <- c(result, correlation)
        }
        
        result
}