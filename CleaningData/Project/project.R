data.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
data.file <- "harDataSet.zip"
download.file(data.url, data.file)
unzip(data.file)

data.train.x <- read.table('UCI HAR Dataset/train/X_train.txt')
data.train.y <- read.table('UCI HAR Dataset/train/Y_train.txt')

data.test.x <- read.table('UCI HAR Dataset/test/X_test.txt')
data.test.y <- read.table('UCI HAR Dataset/test/Y_test.txt')

data.features <- read.table('UCI HAR Dataset/features.txt')
data.features$V2

colnames(data.train.x) <- data.features$V2
colnames(data.test.x) <- data.features$V2
