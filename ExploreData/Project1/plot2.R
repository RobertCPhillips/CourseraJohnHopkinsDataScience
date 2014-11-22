#plot2.R to construct plot2.png

#original file has been pre-processed to only include required data
data <- read.csv("household_power_consumption_small.txt", header=T, sep=";")
data$DateTime <- strptime(paste(data$Date, data$Time), format="%d/%m/%Y %T")

ylab <- "Global Active Power (kilowatts)"

png(filename="plot2.png", width=480, height=480)

plot(data$DateTime, data$Global_active_power,type="l",xlab="",ylab=ylab)

dev.off()