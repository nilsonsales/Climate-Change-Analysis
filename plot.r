# Loading ggplot2 library
library(ggplot2)

# Loading data
row_data <- read.table("Projects/R/Data Science/Climate Change/GlobalLandTemperaturesByCountry.csv", quote="", header=TRUE, sep=",")

# Removing the empty lines
temperature <- row_data[complete.cases(row_data),]
head(temperature)

# Converting dates to the specific date type
temperature$dt <- as.Date(temperature$dt, format="%Y-%m-%d")


# Adding a new column 'Year'
Year <- format(temperature$dt, "%Y")
temperature$Year <- Year



##### Analysing Brazil, US, UK, Japan and South Africa's temperatures #####


## Brazil
tempBrazil <- temperature[ which(temperature$Country=="Brazil"),]

ggplot(data = tempBrazil) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature") +
  ggtitle("Brazil Average Temperature 1932-2013") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")

# Getting the average temperature per year
tempBrazil <- aggregate(x = tempBrazil, 
                        by = list(year = substr(tempBrazil$dt, 1, 4)),
                        FUN = mean)

ggplot(data = tempBrazil) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature") +
  ggtitle("Brazil Average Temperature 1932-2013") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")


## United States
tempUS = temperature[ which(temperature$Country=="United States"),]

tempUS <- aggregate(x = tempUS, 
                    by = list(year = substr(tempUS$dt, 1, 4)),
                    FUN = mean)

# Removing the outliers
lowerWhisker <- boxplot(tempUS[3])$stats[c(1, 5), ][1]
upperWhisker <- boxplot(tempUS[3])$stats[c(1, 5), ][2]

tempUS = tempUS[ which(tempUS$AverageTemperature >=  lowerWhisker & tempUS$AverageTemperature <= upperWhisker),]


ggplot(data = tempUS) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature") +
  ggtitle("US Average Temperature 1768-2013") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")


## United Kingdom
tempUK = temperature[ which(temperature$Country=="United Kingdom"),]

tempUK <- aggregate(x = tempUK, 
                    by = list(year = substr(tempUK$dt, 1, 4)),
                    FUN = mean)

# Removing the outliers
lowerWhisker <- boxplot(tempUK[3])$stats[c(1, 5), ][1]
upperWhisker <- boxplot(tempUK[3])$stats[c(1, 5), ][2]

tempUK = tempUK[ which(tempUK$AverageTemperature >=  lowerWhisker & tempUK$AverageTemperature <= upperWhisker),]


ggplot(data = tempUK) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature") +
  ggtitle("UK Average Temperature 1743-2013") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")


## Japan
tempJapan = temperature[ which(temperature$Country=="Japan"),]

tempJapan <- aggregate(x = tempJapan, 
                       by = list(year = substr(tempJapan$dt, 1, 4)),
                       FUN = mean)

ggplot(data = tempJapan) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature") +
  ggtitle("Japan Average Temperature 1841-2013") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")


## South Africa
tempSA = temperature[ which(temperature$Country=="South Africa"),]

tempSA <- aggregate(x = tempSA, 
                    by = list(year = substr(tempSA$dt, 1, 4)),
                    FUN = mean)

ggplot(data = tempSA) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature") +
  ggtitle("South Africa Average Temperature 1857-2013") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")
