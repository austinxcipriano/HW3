#In-Class Prompts
install.packages(c("dplyr", "lubridate", "tidyverse"))
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)

ClimateChange <- read.csv("/cloud/project/climate-change.csv")
CarbonEmissions <- read.csv("/cloud/project/annual-co-emissions-by-region.csv")

#Prompt 1: Make a plot of air temperature anomalies in the Northern and Southern 
#Hemisphere in base R and in ggplot2.

ClimateChange$Day <- as.Date(ClimateChange$Day)

NorthernHemisphere <- ClimateChange %>% filter(Entity == "Northern Hemisphere")
SouthernHemisphere <- ClimateChange %>% filter(Entity == "Southern Hemisphere")

plot(NorthernHemisphere$Day,
     NorthernHemisphere$temperature_anomaly,
     type = "b",
     col = "darkred",
     ylab = "Temperature Anomalies in the Northern and Southern Hemispheres",
     xlab = "Date",
)

points(SouthernHemisphere$Day,
       SouthernHemisphere$temperature_anomaly,
       type = "b",
       col= "darkblue")

ClimateChangeHemispheres <- ClimateChange %>% filter(!(Entity == "World"))

ggplot(data = ClimateChangeHemispheres, aes(x = Day, y = temperature_anomaly, color = Entity)) +
  geom_point(alpha = .3) +
  labs(x = "Day", y = "Temperature Anomaly")

#Prompt 2: Plot the total all time emissions for the United States, Mexico, and 
#Canada.
colnames(CarbonEmissions)[4] <- "CarbonEmissions"

USAMexicoCanada <- filter(CarbonEmissions, Entity %in% c("United States","Mexico", "Canada")) %>%
  group_by(Entity) 

colnames(USAMexicoCanada)[4] <- "CarbonEmissions"

ggplot(data = USAMexicoCanada, aes(x = Year, y = CarbonEmissions, color = Entity)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Carbon Emissions") +
  theme_classic()

#Homework Prompts
#Prompt 1: Make a graph that communicates about emissions from any countries of 
#your choice. Explain how you considered principles of visualization in making 
#your graph.

ThailandIceland <- CarbonEmissions %>%
  filter(Entity == "Thailand" | Entity == "Iceland")

ggplot(data = ThailandIceland, aes(x = Year, ymin = 0, ymax = CarbonEmissions/1000000, fill = Entity)) +
  geom_ribbon(alpha = .5) +
  xlab("Year") +
  ylab(bquote(~CO[2]~"Emissions (Millions of Tons)")) +
  labs(caption = "Source: Our World in Data") +
  theme_classic() +
  scale_fill_manual(values = c("Iceland" = "blue", "Thailand" = "red"))

#Prompt 2: You are tasked with communicating the change in world air 
#temperatures and CO2 emissions to a broad audience in visually appealing graphs. 
#Make two graphs to present in your word document side by side. Plot world CO2 
#emissions on one graph and world air temperature anomalies on the other graph.

ClimateChange <- ClimateChange %>%
  group_by(Day) %>%
  mutate(avg_temp = mean(temperature_anomaly))

ggplot(data = ClimateChange, aes(x = Day, y = avg_temp)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Date", 
       y = "Global Temperature Anomalies", 
       caption = "Source: Our World in Data") +
  theme_classic()

World <- CarbonEmissions %>%
  filter(Entity == "World")

ggplot(data = World, aes(x = Year, y = CarbonEmissions/1000000000)) +
  geom_line() +
  labs(x = "Year", 
       y = expression(CO[2] ~ "Emissions (Billions of Tons)"),
       caption = "Source: Our World in Data") +
  theme_classic()

#Prompt 3: Look up any type of environmental data of your interest in our world 
#in data (link in tutorial). Download the csv and upload it to RStudio Cloud. 
#Remake the graph. You may make the graph exactly as it is or alter it to present 
#the data in a different format. Explain how you considered principles of 
#visualization in making your graph. Explain the main conclusion of the graph.

LongTermLandUse <- read.csv("/cloud/project/land-use-over-the-long-term.csv")

Thailand <- LongTermLandUse %>%
  filter(Entity == "Thailand") %>%
  filter(Year >= 1600)

ggplot(data = Thailand, aes(x = Year, ymin = 0, ymax = Land.use..Built.up.area/1000)) +
  geom_ribbon(fill = "blue", alpha = .5) +
  xlab("Year") +
  ylab("Built Up Land Area (in millions of ha)") +
  labs(caption = "Source: Our World in Data") +
  theme_classic()