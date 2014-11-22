#plot3.R to construct plot3.png

#original file has been pre-processed to only include required data
data <- read.csv("household_power_consumption_small.txt", header=T, sep=";")
data$DateTime <- strptime(paste(data$Date, data$Time), format="%d/%m/%Y %T")

ylab <- "Energy sub metering"

png(filename="plot3.png", width=480, height=480)

plot(data$DateTime, data$Sub_metering_1,type="l",xlab="",ylab=ylab)
lines(data$DateTime, data$Sub_metering_2, col="red")
lines(data$DateTime, data$Sub_metering_3, col="blue")

legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1),col=c("black","red","blue"))

dev.off()