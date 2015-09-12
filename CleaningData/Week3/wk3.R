
#--------------------------
# q1
#--------------------------
q1.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
q1.file <- "ss06hid.csv"
download.file(q1.url,q1.file)
q1.data <- read.csv(q1.file)
q1.logic <- q1.data$ACR == 3 & q1.data$AGS == 6
which(q1.logic)

#--------------------------
# q2
#--------------------------
require(jpeg)
q2.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
q2.file <- "jeff.jpg"
download.file(q2.url,q2.file,mode="wb")
q2.jpeg <- readJPEG(q2.file, native=T)
q2 <- quantile(q2.jpeg, probs=c(.3,.8))

#--------------------------
# q3
#--------------------------
q3.url1 = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
q3.file1 = "GDP.csv"
q3.url2 = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
q3.file2 = "EDSTATS_Country.csv"
download.file(q3.url1,q3.file1)
download.file(q3.url2,q3.file2)
q3.gdp <- read.csv(q3.file1,header=F,skip=5,
                   col.names = c("CountryCode","Ranking","v1","Country","GDP","v2","v3","v4","v5","v6"))
q3.ed <- read.csv(q3.file2)
q3.gdp.t <- q3.gdp[1:190, c("CountryCode","Ranking","Country","GDP")]
q3.gdp.t$Ranking <- as.numeric(as.character(q3.gdp.t$Ranking))

q3.merged <- merge(q3.ed, q3.gdp.t, by="CountryCode")
head(q3.merged[order(q3.merged$Ranking, decreasing = T),],13)

#--------------------------
# q4
#--------------------------
q4.oecd <- mean(subset(q3.merged, Income.Group == "High income: OECD")$Ranking)
q4.nonoecd <- mean(subset(q3.merged, Income.Group == "High income: nonOECD")$Ranking)
#High income: nonOECD
#High income: OECD

#--------------------------
# q5
#--------------------------
require(dplyr)
q3.merged$q <- ntile(q3.merged$Ranking, 5)
table(q3.merged$Income.Group, q3.merged$q)
