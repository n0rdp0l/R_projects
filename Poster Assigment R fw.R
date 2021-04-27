########## Poster assignment Advanced methods in Economics and Geography ############# 

# Necessary packages
library(wordcloud2)
library(stringi)
library(tm)
library(dplyr)
library(usmap)
library(ggplot2)
library(readr)
library(ggjoy)
library(hrbrthemes)
library(ggpubr)


# Data files
US_Accidents <- read_csv("~/Desktop/Uni/Economics/Year 3/Advanced_geo/project/US_Accidents_June20.csv")
View(US_Accidents)

I = runif(dim(US_Accidents)[1])>0.9
Test = US_Accidents[I,]

Fips_data_file <- read_csv("~/Desktop/Uni/Economics/Year 3/Advanced_geo/project/Fips data file.csv")
View(Fips_data_file)

# Create a fips and state dataframe
FIPS <- data.frame(State = Fips_data_file[,2],
                   fips = Fips_data_file[,4]
)
colnames(FIPS) = c('State', 'fips')


# Merge the fips and state dataframe with the original US_accidents dataframe 
US_Accidents = merge(US_Accidents, FIPS, by = 'State')

#attach variables
attach(US_Accidents)

# Weather descriptions data
WordDescriptions = US_Accidents$Weather_Condition

# Wordcloud all severities
WordsWeather = WordDescriptions[stri_detect_fixed(US_Accidents$Country, 'US')]
WordsWeather = unlist(WordDescriptions)
WordsWeather = data.frame(table(WordsWeather))
wordcloud2(WordsWeather)

# Wordcloud severity 1
WordsWeather.1 =WordDescriptions[stri_detect_fixed(US_Accidents$Severity, '1')]
WordsWeather.1 =unlist(WordsWeather.1)
WordsWeather.1 =data.frame(table(WordsWeather.1))
wordcloud2(WordsWeather.1)

# Wordcloud severity 2
WordsWeather.2 =WordDescriptions[stri_detect_fixed(US_Accidents$Severity, '2')]
WordsWeather.2 =unlist(WordsWeather.2)
WordsWeather.2 =data.frame(table(WordsWeather.2))
wordcloud2(WordsWeather.2)

# Wordcloud severity 3 
WordsWeather.3 =WordDescriptions[stri_detect_fixed(US_Accidents$Severity, '3')]
WordsWeather.3 =unlist(WordsWeather.3)
WordsWeather.3 =data.frame(table(WordsWeather.3))
wordcloud2(WordsWeather.3)

# Wordcloud severity 4
WordsWeather.4 =WordDescriptions[stri_detect_fixed(US_Accidents$Severity, '4')]
WordsWeather.4 =unlist(WordsWeather.4)
WordsWeather.4 =data.frame(table(WordsWeather.4))
wordcloud2(WordsWeather.4)

# Subsets based on severity of the accident
US_accidents_Severity.1 <- subset(US_Accidents, US_Accidents$Severity == 1)
US_accidents_Severity.2 <- subset(US_Accidents, US_Accidents$Severity == 2)
US_accidents_Severity.3 <- subset(US_Accidents, US_Accidents$Severity == 3)
US_accidents_Severity.4 <- subset(US_Accidents, US_Accidents$Severity == 4)

# OLS visibility on frequency of accidents
US_Accidents_Visibility.1 = US_Accidents$`Visibility(mi)`
Visibility.Freq = US_Accidents_Visibility.1[stri_detect_fixed(US_Accidents$Country, 'US')]
Visibility.Freq = unlist(Visibility.Freq)
Visibility.Freq = data.frame(table(Visibility.Freq))

colnames(Visibility.Freq) <- c("Visibility", "Freq")

OLS.Visbility = lm(Visibility.Freq$Freq ~ Visibility.Freq$Visibility, data = Visibility.Freq)
summary(OLS.Visbility)
########## FAILED! ##########

# OLS severity on visibility
OLS.Severity.On.Visibility = lm(Severity ~ `Visibility(mi)`, data=US_Accidents)
summary(OLS.Severity.On.Visibility)

# Create a base map of the US
state_map <- us_map(regions = "states")
plot_usmap("states", labels = TRUE)

# Create database of accidents per state
US_accidents_State = US_Accidents$fips
Accidents.Per.State = US_accidents_State[stri_detect_fixed(US_Accidents$Country, 'US')]
Accidents.Per.State = unlist(Accidents.Per.State)
Accidents.Per.State = data.frame(table(Accidents.Per.State))
colnames(Accidents.Per.State) <- c("fips", "values")

# Create database of accidents per county
US_accidents_County = US_Accidents$County
Accidents.Per.County = US_accidents_County[stri_detect_fixed(US_Accidents$Country, 'US')]
Accidents.Per.County = unlist(Accidents.Per.State)
Accidents.Per.County = data.frame(table(Accidents.Per.State))
colnames(Accidents.Per.County) <- c("county", "values")

US.heatmap = plot_usmap(regions = "state")
US.heatmap = plot_usmap(data = Accidents.Per.State, values = "values", labels = TRUE)

# Bar chart
Severities = US_Accidents$Severity
Severity.Frequency.Table = Severities[stri_detect_fixed(US_Accidents$Country, 'US')]
Severity.Frequency.Table = unlist(Severity.Frequency.Table)
Severity.Frequency.Table = data.frame(table(Severity.Frequency.Table))
colnames(Severity.Frequency.Table) <- c("severity", "frequency")

Severity.Frequency.Table.2 = data.frame(severity = Severity.Frequency.Table$severity,
                                        frequency = as.numeric(Severity.Frequency.Table$frequency/1000))

Chart <- ggplot(data = Severity.Frequency.Table.2, aes(x = severity, y = frequency / 1000, fill = severity)) +
  geom_bar(stat = "identity", fill="steelblue") +
  theme(legend.position="none") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Severity frequency chart")
Chart


# Subsets based on severity of the accident
US_accidents_Severity.1 <- subset(US_Accidents, US_Accidents$Severity == 1)
US_accidents_Severity.1.weathercondition = US_accidents_Severity.1$Weather_Condition
Severity.Weathercondition.1 = US_accidents_Severity.1.weathercondition[stri_detect_fixed(US_accidents_Severity.1$Country, 'US')]
Severity.Weathercondition.1 = unlist(Severity.Weathercondition.1)
Severity.Weathercondition.1 = data.frame(table(Severity.Weathercondition.1))
colnames(Severity.Weathercondition.1) <- c("weathercondition", "frequency")
Severity.Weathercondition.1[order(-Severity.Weathercondition.1$frequency),]
### 5 most frequent: Fair, Mostly Cloudy, Cloudy, Partly Cloudy, Light Rain
Severity.Weathercondition.1.Selection <- Severity.Weathercondition.1[c(7, 30, 4, 33, 21), c("weathercondition", "frequency")]
Severity.Weathercondition.1.Selection.Freq = data.frame(weathercondition = Severity.Weathercondition.1.Selection$weathercondition,
                                                        frequency = as.numeric(Severity.Weathercondition.1.Selection$frequency/29174))


US_accidents_Severity.2 <- subset(US_Accidents, US_Accidents$Severity == 2)
US_accidents_Severity.2.weathercondition = US_accidents_Severity.2$Weather_Condition
Severity.Weathercondition.2 = US_accidents_Severity.2.weathercondition[stri_detect_fixed(US_accidents_Severity.2$Country, 'US')]
Severity.Weathercondition.2 = unlist(Severity.Weathercondition.2)
Severity.Weathercondition.2 = data.frame(table(Severity.Weathercondition.2))
colnames(Severity.Weathercondition.2) <- c("weathercondition", "frequency")
Severity.Weathercondition.2[order(-Severity.Weathercondition.2$frequency),]
### 5 most frequent: Clear, Fair, Mostly Cloudy, Overcast, Partly Cloudy
Severity.Weathercondition.2.Selection <- Severity.Weathercondition.2[c(5, 13, 71, 74, 76), c("weathercondition", "frequency")]
Severity.Weathercondition.2.Selection.Freq = data.frame(weathercondition = Severity.Weathercondition.2.Selection$weathercondition,
                                                        frequency = as.numeric(Severity.Weathercondition.2.Selection$frequency/2373210))


US_accidents_Severity.3 <- subset(US_Accidents, US_Accidents$Severity == 3)
US_accidents_Severity.3.weathercondition = US_accidents_Severity.3$Weather_Condition
Severity.Weathercondition.3 = US_accidents_Severity.3.weathercondition[stri_detect_fixed(US_accidents_Severity.3$Country, 'US')]
Severity.Weathercondition.3 = unlist(Severity.Weathercondition.3)
Severity.Weathercondition.3 = data.frame(table(Severity.Weathercondition.3))
colnames(Severity.Weathercondition.3) <- c("weathercondition", "frequency")
Severity.Weathercondition.3[order(-Severity.Weathercondition.3$frequency),]
### 5 most frequent: Clear, Mostly Cloudy, Overcast, Fair, Partly Cloudy
Severity.Weathercondition.3.Selection <- Severity.Weathercondition.3[c(6, 69, 72, 12, 75), c("weathercondition", "frequency")]
Severity.Weathercondition.3.Selection.Freq = data.frame(weathercondition = Severity.Weathercondition.3.Selection$weathercondition,
                                                        frequency = as.numeric(Severity.Weathercondition.3.Selection$frequency/998913))


US_accidents_Severity.4 <- subset(US_Accidents, US_Accidents$Severity == 4)
US_accidents_Severity.4.weathercondition = US_accidents_Severity.4$Weather_Condition
Severity.Weathercondition.4 = US_accidents_Severity.4.weathercondition[stri_detect_fixed(US_accidents_Severity.4$Country, 'US')]
Severity.Weathercondition.4 = unlist(Severity.Weathercondition.4)
Severity.Weathercondition.4 = data.frame(table(Severity.Weathercondition.4))
colnames(Severity.Weathercondition.4) <- c("weathercondition", "frequency")
Severity.Weathercondition.4[order(-Severity.Weathercondition.4$frequency),]
### 5 most frequent: Clear, Fair, Mostly Cloudy, Overcast, Partly Cloudy
Severity.Weathercondition.4.Selection <- Severity.Weathercondition.4[c(4, 9, 49, 52, 53), c("weathercondition", "frequency")]
Severity.Weathercondition.4.Selection.Freq = data.frame(weathercondition = Severity.Weathercondition.4.Selection$weathercondition,
                                                        frequency = as.numeric(Severity.Weathercondition.4.Selection$frequency/112320))



# Create the 4 bar charts for the weatherconditions
plot.s.1 <- ggplot(data = Severity.Weathercondition.1.Selection.Freq, aes(x = weathercondition, y = frequency, fill = weathercondition)) +
  geom_bar(stat = "identity", fill="steelblue") +
  theme(legend.position="none") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Severity 1") +
  ylim(0, 0.50) +
  ylab('percentage of total')
plot.s.1

plot.s.2 <- ggplot(data = Severity.Weathercondition.2.Selection.Freq, aes(x = weathercondition, y = frequency, fill = weathercondition)) +
  geom_bar(stat = "identity", fill="steelblue") +
  theme(legend.position="none") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Severity 2") +
  ylim(0, 0.50) +
  ylab('percentage of total')
plot.s.2

plot.s.3 <- ggplot(data = Severity.Weathercondition.3.Selection.Freq, aes(x = weathercondition, y = frequency, fill = weathercondition)) +
  geom_bar(stat = "identity", fill="steelblue") +
  theme(legend.position="none") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Severity 3") +
  ylim(0, 0.50) +
  ylab('percentage of total')
plot.s.3

plot.s.4 <- ggplot(data = Severity.Weathercondition.4.Selection.Freq, aes(x = weathercondition, y = frequency, fill = weathercondition)) +
  geom_bar(stat = "identity", fill="steelblue") +
  theme(legend.position="none") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Severity 4") +
  ylim(0, 0.50) +
  ylab('percentage of total')
plot.s.4

# Create 2 by 2 setup
plot_grid(plot.s.1, plot.s.2, plot.s.3, plot.s.4)





p <- ggplot(US_Accidents, aes(x = Start_Lng,
                              y = Start_Lat,
                              colour = factor(Severity), 
                              )
            )

p + geom_point(alpha=1/sqrt(nrow(US_Accidents))) +geom_point(colour = factor(Severity))+ geom_point(size = 0.5)

p + geom_point(alpha=1/5,
               size = 0.5) 

ggplot(US_Accidents,aes(x = `Visibility(mi)`,y=Severity,))+
  geom_joy(scale = 3, rel_min_height = 0.01) +
  theme_ipsum(grid=F)+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(angle = 180, hjust = 1))



### Test Dataset
boxplot(Test$`Visibility(mi)`,
        ylab = "Visibility(mi)`"
)

boxplot(Test$`Wind_Speed(mph)`,
        ylab = "`Wind_Speed(mph)`"
)



Test_wind_clean = subset(Test, Test$`Wind_Speed(mph)` <= 100) #controlling for outliers
Test_visibility_clean = subset(Test, Test$`Visibility(mi)` <= 60)

ggplot(Test_visibility_clean, aes(x = `Visibility(mi)` ,y=Severity, group = Severity)) + geom_density_ridges2()
ggplot(Test_wind_clean, aes(x = `Wind_Speed(mph)` ,y=Severity, group = Severity)) + geom_density_ridges2()

### Real Dataset
US_Accidents_wind_clean = subset(US_Accidents, US_Accidents$`Wind_Speed(mph)` <= 65) #controlling for outliers
US_Accidents_visibility_clean = subset(US_Accidents, US_Accidents$`Visibility(mi)` <= 30)

par(mfrow=c(2,1))

A = ggplot(US_Accidents_visibility_clean, aes(x = `Visibility(mi)` ,y=Severity, group = Severity)) + geom_density_ridges2()
B = ggplot(US_Accidents_wind_clean, aes(x = `Wind_Speed(mph)` ,y=Severity, group = Severity)) + geom_density_ridges2()

ggarrange(A, B, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)



