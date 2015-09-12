
#--------------------------
# q1
#--------------------------
q1.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
q1.file <- "ss06hid.csv"
download.file(q1.url, q1.file)
q1.data <- read.csv(q1.file)
q1 <- strsplit(names(q1.data), "wgtp")
#--------------------------
# q2
#--------------------------
q2.url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
q2.file = "GDP.csv"
download.file(q2.url, q2.file)
q2.data <- read.csv(q2.file, header=F, skip=5,
                    col.names = c("CountryCode","Ranking","v1","Country","GDP","v2","v3","v4","v5","v6"))

q2.data.t <- q2.data[1:190, c("CountryCode","Ranking","Country","GDP")]

q2 <- mean(as.numeric(gsub(",", "", as.character(q2.data.t$GDP))))

#--------------------------
# q3
#--------------------------
grep("^United",q2.data.t$Country)


#--------------------------
# q4
#--------------------------
q4.url1 = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
q4.file1 = "GDP.csv"
q4.url2 = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
q4.file2 = "EDSTATS_Country.csv"
download.file(q4.url1,q4.file1)
download.file(q4.url2,q4.file2)
q4.gdp <- read.csv(q4.file1,header=F,skip=5,
                   col.names = c("CountryCode","Ranking","v1","Country","GDP","v2","v3","v4","v5","v6"))
q4.ed <- read.csv(q4.file2)
q4.gdp.t <- q4.gdp[1:190, c("CountryCode","Ranking","Country","GDP")]
q4.gdp.t$Ranking <- as.numeric(as.character(q4.gdp.t$Ranking))

q4.merged <- merge(q4.ed, q4.gdp.t, by="CountryCode")
head(q4.merged[order(q4.merged$Ranking, decreasing = T),],13)

q4 <- grep("^Fiscal Year End: June",q4.merged$Special.Notes, ignore.case=TRUE)

#--------------------------
# q5
#--------------------------
require(quantmod)
amzn <- getSymbols("AMZN",auto.assign=FALSE)
sampleTimes <- index(amzn) 
q5a <- sum(sampleTimes >= "2012-01-01" & sampleTimes <= "2012-12-31")
q5b <- sum(format(sampleTimes, format='%A') == "Monday" & format(sampleTimes, format='%y') == "12")



