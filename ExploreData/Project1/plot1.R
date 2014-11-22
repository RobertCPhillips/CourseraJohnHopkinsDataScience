#plot1.R to construct plot1.png

#original file has been pre-processed to only include required data
data <- read.csv("household_power_consumption_small.txt", header=T, sep=";")
data$DateTime <- strptime(paste(data$Date, data$Time), format="%d/%m/%Y %T")

xlab <- "Global Active Power (kilowatts)"
title <- "Global Active Power"

png(filename="plot1.png", width=480, height=480)

hist(data$Global_active_power, col="red", xlab=xlab, main=title)

dev.off()