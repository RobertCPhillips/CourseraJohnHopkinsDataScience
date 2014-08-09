#Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. For example, data for monitor 200 is contained in the file "200.csv". Each file contains three variables:

#  Date: the date of the observation in YYYY-MM-DD format (year-month-day)

#  sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)

#  nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)

monitorDataMerge <- function(data1, data2) {
        merge(data1, data2, by=c("Date","sulfate","nitrate", "ID"), all=TRUE)
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
        files <- dir(directory, full.names=TRUE)[id]
        allData = c()        
        
        for (f in files) {
                fileData = read.csv(f)
                columnData = fileData[pollutant]
                allData = c(allData, columnData[!is.na(columnData)])
        }
        
        mean(allData)
}

#first version...too slow
pollutantmeanOrig <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        files <- dir(directory, full.names=TRUE)[id]
        fileList <- lapply(files, read.csv)
        
        dataInFiles <- Reduce(monitorDataMerge, fileList)
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        pData = dataInFiles[pollutant]
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        mean(pData[!is.na(pData)])
}