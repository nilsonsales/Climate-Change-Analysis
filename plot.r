# Loading ggplot2 library
library(ggplot2)


# Defining multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##########


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


#### Now lets separete it per season and compare
#### two countries in different hemispheres

## Brazil

# Reload the full dataset
tempBrazil <- temperature[ which(temperature$Country=="Brazil"),]

# Creating subsets with the seasons
summer <- tempBrazil[ which( as.numeric(format(tempBrazil$dt, "%m")) == 12 | as.numeric(format(tempBrazil$dt, "%m")) <= 2 ),]
summer <- aggregate(x = summer, 
                    by = list(year = substr(summer$dt, 1, 4)),
                    FUN = mean)

fall <- tempBrazil[ which( as.numeric(format(tempBrazil$dt, "%m")) > 2 & as.numeric(format(tempBrazil$dt, "%m")) <= 5 ),]
fall <- aggregate(x = fall, 
                  by = list(year = substr(fall$dt, 1, 4)),
                  FUN = mean)

winter <- tempBrazil[ which( as.numeric(format(tempBrazil$dt, "%m")) > 5 & as.numeric(format(tempBrazil$dt, "%m")) <= 8 ),]
winter <- aggregate(x = winter, 
                    by = list(year = substr(winter$dt, 1, 4)),
                    FUN = mean)

spring <- tempBrazil[ which( as.numeric(format(tempBrazil$dt, "%m")) > 8 & as.numeric(format(tempBrazil$dt, "%m")) <= 11 ),]
spring <- aggregate(x = spring, 
                    by = list(year = substr(spring$dt, 1, 4)),
                    FUN = mean)


# Saving the plots
p1 <- ggplot(data = summer) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle("Summer") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")

p2 <- ggplot(data = fall) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="orange") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle("Fall") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")

p3 <- ggplot(data = winter) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="cyan1") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle("Winter") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")

p4 <- ggplot(data = spring) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="yellow") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle("Spring") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")

# Plotting
multiplot(p1, p2, p3, p4, cols=2)



### United Kingdom

tempUK <- temperature[ which(temperature$Country=="United Kingdom"),]

# Creating subsets with the seasons
winter <- tempUK[ which( as.numeric(format(tempUK$dt, "%m")) == 12 | as.numeric(format(tempUK$dt, "%m")) <= 2 ),]
winter <- aggregate(x = winter, 
                    by = list(year = substr(winter$dt, 1, 4)),
                    FUN = mean)

spring <- tempUK[ which( as.numeric(format(tempUK$dt, "%m")) > 2 & as.numeric(format(tempUK$dt, "%m")) <= 5 ),]
spring <- aggregate(x = spring, 
                    by = list(year = substr(spring$dt, 1, 4)),
                    FUN = mean)

summer <- tempUK[ which( as.numeric(format(tempUK$dt, "%m")) > 5 & as.numeric(format(tempUK$dt, "%m")) <= 8 ),]
summer <- aggregate(x = summer, 
                    by = list(year = substr(summer$dt, 1, 4)),
                    FUN = mean)

fall <- tempUK[ which( as.numeric(format(tempBrazil$dt, "%m")) > 8 & as.numeric(format(tempUK$dt, "%m")) <= 11 ),]
fall <- aggregate(x = fall, 
                  by = list(year = substr(fall$dt, 1, 4)),
                  FUN = mean)


# Saving the plots
p1 <- ggplot(data = summer) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="red") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle("Summer") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")

p2 <- ggplot(data = fall) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="orange") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle("Fall") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")

p3 <- ggplot(data = winter) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="cyan1") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle("Winter") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")

p4 <- ggplot(data = spring) + 
  geom_point(mapping =  aes(x = dt, y = AverageTemperature, colour=AverageTemperature)) +
  geom_smooth(mapping = aes(x = dt, y = AverageTemperature)) +
  scale_color_gradient(low="blue", high="yellow") +
  xlab("Year") + ylab("Temperature (°C)") +
  ggtitle("Spring") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(colour = "Temp")

# Plotting
multiplot(p1, p2, p3, p4, cols=2)
