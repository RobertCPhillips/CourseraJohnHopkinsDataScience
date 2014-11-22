#plot4.R to construct plot4.png

#original file has been pre-processed to only include required data
data <- read.csv("household_power_consumption_small.txt", header=T, sep=";")
data$DateTime <- strptime(paste(data$Date, data$Time), format="%d/%m/%Y %T")

png(filename="plot4.png", width=480, height=480)
par(mfrow = c(2,2))

ylab <- "Global Active Power"
plot(data$DateTime, data$Global_active_power,type="l",xlab="",ylab=ylab)

ylab <- "Voltage"
xlab <- "datetime"
plot(data$DateTime, data$Voltage,type="l",xlab=xlab,ylab=ylab)

ylab <- "Energy sub metering"
plot(data$DateTime, data$Sub_metering_1,type="l",xlab="",ylab=ylab)
lines(data$DateTime, data$Sub_metering_2, col="red")
lines(data$DateTime, data$Sub_metering_3, col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1),col=c("black","red","blue"))

ylab <- "Global_reactive_power"
xlab <- "datetime"
plot(data$DateTime, data$Global_reactive_power,type="l",xlab=xlab,ylab=ylab)

dev.off()