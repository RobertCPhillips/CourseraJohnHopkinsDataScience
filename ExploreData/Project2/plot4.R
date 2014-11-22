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

#Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
years <- c(1999, 2002, 2005, 2008)
coalSectors <- SCC[grep("[Cc]oal",SCC$EI.Sector),"SCC"]

yearCoalCounts <- sapply(years, function(y) {
  sum(NEI[NEI$year == y & NEI$SCC %in% coalSectors, "Emissions"])
})
fit <- lm(yearCoalCounts~years)

png(filename="plot4.png", width=480, height=480)

plot(years, yearCoalCounts, main="Coal Combustion Emissions (PM2.5)", xlab="Years",
     ylab="Emissions in Tons", type="p", xaxt="n")
points(years, y=yearCoalCounts, pch=19, col = "dark red")
abline(reg=fit) 
axis(1, at=years)

dev.off()

