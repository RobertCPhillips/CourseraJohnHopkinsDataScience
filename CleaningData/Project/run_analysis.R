#------------------------
# download and unzip
#------------------------
data.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
data.file <- "harDataSet.zip"
download.file(data.url, data.file)
unzip(data.file)

# load training data
data.train.x <- read.table('UCI HAR Dataset/train/X_train.txt')
data.train.y <- read.table('UCI HAR Dataset/train/Y_train.txt')
data.train.sub <- read.table('UCI HAR Dataset/train/subject_train.txt')

#load test data
data.test.x <- read.table('UCI HAR Dataset/test/X_test.txt')
data.test.y <- read.table('UCI HAR Dataset/test/Y_test.txt')
data.test.sub <- read.table('UCI HAR Dataset/test/subject_test.txt')

#load the activities and features files, use features as column names
data.activities <- read.table('UCI HAR Dataset/activity_labels.txt', stringsAsFactors=F)
data.features <- read.table('UCI HAR Dataset/features.txt', stringsAsFactors=F)
feature.names <- data.features$V2

#get the feature names for mean and standard dev.
feature.names.meanOrStd <- feature.names[c(grep("mean", feature.names),
                                           grep("std",feature.names))]

colnames(data.train.x) <- feature.names
colnames(data.test.x) <- feature.names

#---------------------------------------
#merge the training and test data
#---------------------------------------
require(dplyr)
data.train.y.labeled <- left_join(data.train.y, data.activities, by="V1")
data.test.y.labeled <- left_join(data.test.y, data.activities, by="V1")

data.train.x$subject <- data.train.sub$V1
data.test.x$subject <- data.test.sub$V1

data.train.x$activity <- data.train.y.labeled$V2
data.test.x$activity <- data.test.y.labeled$V2

data.all.names <- c(feature.names.meanOrStd, "subject", "activity")
data.all <- rbind(data.train.x[,data.all.names], 
                  data.test.x[,data.all.names])

#----------------------------------------
#summarize by subject and activity
#----------------------------------------
data.summary <- ddply(data.all, 
                      .(subject,activity), 
                      function(x){colMeans(x[feature.names.meanOrStd])})

#----------------------------------------
#output tidy file
#----------------------------------------
write.table(data.summary, file="data.summary.txt", row.names=F)
