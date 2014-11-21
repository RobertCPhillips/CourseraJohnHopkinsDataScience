library(ggplot2)
library(gridExtra)

#Contains number of tons of PM2.5 emitted from a specific type of source for the entire year.
NEI <- readRDS("summarySCC_PM25.rds")

#fips: A five-digit number (represented as a string) indicating the U.S. county
#SCC: The name of the source as indicated by a digit string (see source code classification table)
#Pollutant: A string indicating the pollutant
#Emissions: Amount of PM2.5 emitted, in tons
#type: The type of source (point, non-point, on-road, or non-road)
#year: The year of emissions recorded

#Provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source
SCC <- readRDS("Source_Classification_Code.rds")

#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? 
Use the ggplot2 plotting system to make a plot answer this question.
years <- c(1999, 2002, 2005, 2008)
types <- c("POINT", "NONPOINT", "ON-ROAD", "NON-ROAD")
yearTypeCounts <- lapply(types, function(t) {
    sapply(years, function(y) sum(NEI[NEI$year == y & NEI$fips == 24510 & NEI$type == t,"Emissions"]))
  })

p1 <- qplot(years, yearTypeCounts[[1]], main="PM2.5 POINT Emissions - Baltimore City, Maryland", xlab="Years",
           ylab="Emissions in Tons", type="p") +
      stat_smooth(method="lm", se=F)

p2 <- qplot(years, yearTypeCounts[[2]], main="PM2.5 NONPOINT Emissions - Baltimore City, Maryland", xlab="Years",
            ylab="Emissions in Tons", type="p") +
      stat_smooth(method="lm", se=F)

p3 <- qplot(years, yearTypeCounts[[3]], main="PM2.5 ON-ROAD Emissions - Baltimore City, Maryland", xlab="Years",
            ylab="Emissions in Tons", type="p") +
      stat_smooth(method="lm", se=F)

p4 <- qplot(years, yearTypeCounts[[4]], main="PM2.5 NON-ROAD Emissions - Baltimore City, Maryland", xlab="Years",
            ylab="Emissions in Tons", type="p") +
      stat_smooth(method="lm", se=F)

png(filename="plot3.png", width=800, height=480)

grid.arrange(p1,p2,p3,p4)

dev.off()



