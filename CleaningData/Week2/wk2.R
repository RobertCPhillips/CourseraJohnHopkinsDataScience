
#----------------------------
# q1
#----------------------------
require(jsonlite)
q1.url = "https://api.github.com/users/jtleek/repos"
q1.data <- fromJSON(q1.url)

q1 <- q1.data[q1.data$name == "datasharing","created_at"]

#----------------------------
# q2 and q3
#----------------------------
require(sqldf)
q2.url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(q2.url,"ss06hid.csv")
q2.acs <- read.csv("ss06hid.csv")
sqldf("select pwgtp1 from [q2.acs] where AGEP < 50", drv="SQLite")
sqldf("select distinct AGEP from [q2.acs]", drv="SQLite")

#----------------------------
# q4
#----------------------------
q4.url = "http://biostat.jhsph.edu/~jleek/contact.html"
q4.conn <- url(q4.url)
q4.html <- readLines(q4.conn)
close(q4.conn)
q4 <- nchar(q4.html[c(10,20,30,100)])

#----------------------------
# q5
#----------------------------
q5.url = "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
download.file(q5.url,"wksst8110.for")

q5.data <- read.fwf(file="wksst8110.for",
                    skip=4,
                    widths=c(-1, 9, -5,4,4, -5,4,4,-5,4,4, -5,4,4),
                    col.names=c("Week","Nino12SST","Nino12SSTA","Nino3SST","Nino3SSTA","Nino34SST","Nino34SSTA","Nino4SST","Nino4SSTA"))

q5 <- colSums(q5.data[c(4,9)])


