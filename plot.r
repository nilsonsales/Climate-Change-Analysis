# Loading ggplot2
library(ggplot2)

# Loading data
row_data <- read.table("Projects/R/Data Science/Climate Change/GlobalLandTemperaturesByCountry.csv", quote="", header=TRUE, sep=",")

# Removing empty lines
temperature <- row_data[complete.cases(row_data),]
head(temperature)

# Converting data to the specific type
temperature$dt <- as.Date(temperature$dt, format="%Y-%m-%d")
temperature$dt


# grouping by contry  
ggplot(data = temperature) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature))


# Plotting Brazil average temperature
tempBrazil <- temperature[ which(temperature$Country=="Brazil"),]

tail(tempBrazil)

ggplot(data = tempBrazil) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Years") + ylab("Temperature") +
  ggtitle("Brazil Average Temperature 1932-2013") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temperature")


# United States
tempUS = temperature[ which(temperature$Country=="United States"),]

ggplot(data = tempUS) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red")


# United Kingdom
tempUK = temperature[ which(temperature$Country=="United Kingdom"),]

ggplot(data = tempUK) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red")
