
#--------------------------
# q1 and q2
#--------------------------
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv","ss06hid.csv")
q1.idaho <- read.csv("ss06hid.csv")
q1 <- sum(q1.idaho$VAL == 24, na.rm=T)

#--------------------------
# q3
#--------------------------
#getdata_data_DATA.gov_NGAP
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx","gov_NGAP.xlsx", mode="wb")
require(xlsx)
q3.colIndex <- 7:15
q3.rowIndex <- 18:23
q3.dat <- read.xlsx("gov_NGAP.xlsx", sheetIndex = 1, colIndex = q3.colIndex, rowIndex = q3.rowIndex)
q3 <- sum(q3.dat$Zip*q3.dat$Ext, na.rm = T)

#--------------------------
# q4
#--------------------------
#getdata_data_restaurants
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml","restaurants.xml")
require(XML)
q4.zip <- 21231
q4.xml <- xmlTreeParse("restaurants.xml", useInternal=T)
q4.root <- xmlRoot(q4.xml)
q4 <- sum(xpathSApply(q4.root, "//zipcode", xmlValue) == "21231")

#--------------------------
# q5
#--------------------------
#getdata_data_ss06pid
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv","ss06pid.csv")
require(data.table)
q5.DT <- fread("ss06pid.csv")
q5 <- q5.DT[,mean(pwgtp15),by=SEX]



