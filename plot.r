# Loading ggplot2 library
library(ggplot2)

# Loading data
row_data <- read.table("Projects/R/Data Science/Climate Change/GlobalLandTemperaturesByCountry.csv", quote="", header=TRUE, sep=",")

# Turning warnings off
options(warn=-1)


### Cleaning and adjusting our data set ###

# Removing the empty lines
temperature <- row_data[complete.cases(row_data),]

# Converting dates to the specific date type
temperature$dt <- as.Date(temperature$dt, format="%Y-%m-%d")

# Taking a look at our data
head(temperature)
tail(temperature)


##### Analysing Brazil, United States, United Kigdom, Japan and South Africa's temperatures #####

## Brazil
tempBrazil <- temperature[ which(temperature$Country=="Brazil"),]

# Generating title with initial and last years
firstYear <- format(tempBrazil$dt[1], "%Y")
lastYear <- format(tempBrazil$dt[length(tempBrazil$dt)], "%Y")

title = paste("Brazil Average Temperature ", firstYear,"-", lastYear, sep="")


ggplot(data = tempBrazil) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")

# Getting the average temperature per year
tempBrazil <- aggregate(x = tempBrazil, 
                        by = list(year = substr(tempBrazil$dt, 1, 4)),
                        FUN = mean)

ggplot(data = tempBrazil) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")


## United States
tempUS = temperature[ which(temperature$Country=="United States"),]

# Generating title with initial and last years
firstYear <- format(tempUS$dt[1], "%Y")
lastYear <- format(tempUS$dt[length(tempUS$dt)], "%Y")

title = paste("US Average Temperature ", firstYear,"-", lastYear, sep="")


# Agreggating the data by year
tempUS <- aggregate(x = tempUS, 
                    by = list(year = substr(tempUS$dt, 1, 4)),
                    FUN = mean)

# Removing the outliers
lowerWhisker <- boxplot(tempUS[3], plot=FALSE)$stats[c(1, 5), ][1]
upperWhisker <- boxplot(tempUS[3], plot=FALSE)$stats[c(1, 5), ][2]

tempUS = tempUS[ which(tempUS$AverageTemperature >=  lowerWhisker & tempUS$AverageTemperature <= upperWhisker),]


ggplot(data = tempUS) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle("US Average Temperature 1768-2013") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")


## United Kingdom
tempUK = temperature[ which(temperature$Country=="United Kingdom"),]

# Generating title with initial and last years
firstYear <- format(tempUK$dt[1], "%Y")
lastYear <- format(tempUK$dt[length(tempUK$dt)], "%Y")

title = paste("UK Average Temperature ", firstYear,"-", lastYear, sep="")


# Agreggating the data by year
tempUK <- aggregate(x = tempUK, 
                    by = list(year = substr(tempUK$dt, 1, 4)),
                    FUN = mean)

# Removing the outliers
lowerWhisker <- boxplot(tempUK[3], plot=FALSE)$stats[c(1, 5), ][1]
upperWhisker <- boxplot(tempUK[3], plot=FALSE)$stats[c(1, 5), ][2]

tempUK = tempUK[ which(tempUK$AverageTemperature >=  lowerWhisker & tempUK$AverageTemperature <= upperWhisker),]


ggplot(data = tempUK) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle("UK Average Temperature 1743-2013") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")


## Japan
tempJapan = temperature[ which(temperature$Country=="Japan"),]

# Generating title with initial and last years
firstYear <- format(tempJapan$dt[1], "%Y")
lastYear <- format(tempJapan$dt[length(tempJapan$dt)], "%Y")

title = paste("Japan Average Temperature ", firstYear,"-", lastYear, sep="")


# Agreggating the data by year
tempJapan <- aggregate(x = tempJapan, 
                       by = list(year = substr(tempJapan$dt, 1, 4)),
                       FUN = mean)


ggplot(data = tempJapan) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle("Japan Average Temperature 1841-2013") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")


## South Africa
tempSA = temperature[ which(temperature$Country=="South Africa"),]

# Generating title with initial and last years
firstYear <- format(tempSA$dt[1], "%Y")
lastYear <- format(tempSA$dt[length(tempSA$dt)], "%Y")

title = paste("South Africa Average Temperature ", firstYear,"-", lastYear, sep="")


# Agreggating the data by year
tempSA <- aggregate(x = tempSA, 
                    by = list(year = substr(tempSA$dt, 1, 4)),
                    FUN = mean)


ggplot(data = tempSA) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle("South Africa Average Temperature 1857-2013") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")
