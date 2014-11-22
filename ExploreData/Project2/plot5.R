
#fips: A five-digit number (represented as a string) indicating the U.S. county
#SCC: The name of the source as indicated by a digit string (see source code classification table)
#Pollutant: A string indicating the pollutant
#Emissions: Amount of PM2.5 emitted, in tons
#type: The type of source (point, non-point, on-road, or non-road)
#year: The year of emissions recorded

#Contains number of tons of PM2.5 emitted from a specific type of source for the entire year.
NEI <- readRDS("summarySCC_PM25.rds")

#Provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source
SCC <- readRDS("Source_Classification_Code.rds")

#How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
years <- c(1999, 2002, 2005, 2008)
sectors <- SCC[grep("Vehicles",SCC$EI.Sector),"SCC"]

yearCounts <- sapply(years, function(y) {
  sum(NEI[NEI$year == y & NEI$fips == 24510 & NEI$SCC %in% sectors, "Emissions"])
})
fit <- lm(yearCounts~years)

png(filename="plot5.png", width=480, height=480)
par(mfrow=c(1,1))

plot(years, yearCounts, main="Motor Vehicle Emissions (PM2.5) - Baltimore City, MD", xlab="Years",
     ylab="Emissions in Tons", type="p", xaxt="n")
points(years, y=yearCounts, pch=19, col = "dark red")
abline(reg=fit) 
axis(1, at=years)

dev.off()

