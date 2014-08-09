#q16
countMissing <- function(hwData, columnName) {
  columnData <- hwData[columnName]
  naCount <- sum(is.na(columnData))
  return (naCount)
}

#q17
columnMean <- function(hwData, columnName) {
  columnData <- hwData[columnName]
  columnValues <- columnData[!is.na(columnData)]
  return (mean(columnValues))
}

#q18
question18 <- function(hwData) {
  validData <- complete.cases(hwData["Ozone"], hwData["Temp"], hwData["Solar.R"])
  filteredData <- hwData[validData & hwData["Ozone"] > 31 & hwData["Temp"] > 90,]
  return (columnMean(filteredData, "Solar.R"))
}

#q19
question19 <- function(hwData) {
  validData <- complete.cases(hwData["Month"], hwData["Temp"])
  filteredData <- hwData[validData & hwData["Month"] == 6, ]
  return (columnMean(filteredData, "Temp"))
}

#q20
question20 <- function(hwData) {
  validData <- complete.cases(hwData["Month"], hwData["Ozone"])
  filteredData <- hwData[validData & hwData["Month"] == 5, ]
  return (max(filteredData["Ozone"]))
}