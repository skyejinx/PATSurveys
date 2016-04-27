library(survey)
buses <- read.csv('bus.csv', header=TRUE, stringsAsFactors = FALSE)
# Initial Exploration

head(buses)
dim(buses)
# 87 rows with 8 columns
str(buses)
summary(buses)
buses$time_of_day <- as.factor(buses$time_of_day)
buses$neighborhood <- as.factor(buses$neighborhood)
str(buses)

## Let's Create the Survey Design Object
table(buses$neighborhood)
# Read in the total number of bus stops in each neighborhood
population <- read.csv('bus_population.csv', stringsAsFactors=FALSE)
buses <- merge(buses, bus_population, by = "neighborhood")

des <- svydesign(ids = ~1, 
                probs = NULL, 
                strata = ~neighborhood, 
                fpc = ~total_stops, 
                data = buses)

summary(des)


# Box plot
#svyboxplot(difference_arrival_rate~neighborhood, des, main="Difference in Observed and Official Arrival Times Across Neighborhoods", 
    )
#svyplot(~difference_arrival_rate, des, main="SRS") 

ggplot(data = buses, aes(x = neighborhood, y = difference_arrival_rate, fill = neighborhood)) + geom_boxplot() + 
  labs(x = "Neighborhood", y = "Difference between Observed and Official Arrival Times") +
      ggtitle("Difference in Observed and Official Arrival Times Across Neighborhoods")

  
# Density Plot
ggplot(buses, aes(x = difference_arrival_rate)) + geom_density() + 
  labs(x = "Difference between Bus Scheduled and Arrival Rates", y = "density") + 
  ggtitle("Distribution of Bus Arrival Rates on Weekend/Weekday") +
  facet_wrap(~weekday) + geom_vline(xintercept = 0, color = "red")

#ggplot(buses, aes(x = difference_arrival_rate)) + geom_density() + facet_wrap(~time_of_day) 
#+ ggtitle("Distribution of Bus Arrival Rates Across Time of Day")

ggplot(data = buses, aes(x = timeofday, y = difference_arrival_rate, fill = timeofday)) + 
  geom_violin() + labs(x = "Neighborhood", y = "Difference between Observed and Official Arrival Times") +
  ggtitle("Difference in Observed and Official Arrival Times Across Times of Day")

#ggplot(data = buses, aes(x = timeofday, y = difference_arrival_rate)) + geom_boxplot()





