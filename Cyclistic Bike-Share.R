library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(dplyr)
install.packages("kimisc")
library(kimisc)

df1 <- read_csv("C:/Users/hp/Documents/Cyclistic Bike Share/All Data/202010-divvy-tripdata.csv")
df2 <- read_csv("C:/Users/hp/Documents/Cyclistic Bike Share/All Data/202011-divvy-tripdata.csv")
df3 <- read_csv("C:/Users/hp/Documents/Cyclistic Bike Share/All Data/202012-divvy-tripdata.csv")
df4 <- read_csv("C:/Users/hp/Documents/Cyclistic Bike Share/All Data/202101-divvy-tripdata.csv")
df5 <- read_csv("C:/Users/hp/Documents/Cyclistic Bike Share/All Data/202102-divvy-tripdata.csv")
df6 <- read_csv("C:/Users/hp/Documents/Cyclistic Bike Share/All Data/202103-divvy-tripdata.csv")
df7 <- read_csv("C:/Users/hp/Documents/Cyclistic Bike Share/All Data/202104-divvy-tripdata.csv")
df8 <- read_csv("C:/Users/hp/Documents/Cyclistic Bike Share/All Data/202105-divvy-tripdata.csv")
df9 <- read_csv("C:/Users/hp/Documents/Cyclistic Bike Share/All Data/202106-divvy-tripdata.csv")
df10 <- read_csv("C:/Users/hp/Documents/Cyclistic Bike Share/All Data/202107-divvy-tripdata.csv")
df11 <- read_csv("C:/Users/hp/Documents/Cyclistic Bike Share/All Data/202108-divvy-tripdata.csv")
df12 <- read_csv("C:/Users/hp/Documents/Cyclistic Bike Share/All Data/202109-divvy-tripdata.csv")

bike_rides <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
bike_rides <- remove_empty(bike_rides,which= c("cols"))
bike_rides <- remove_empty(bike_rides,which= c("rows"))

bike_rides$started_at <- ymd_hms(bike_rides$started_at)
bike_rides$ended_at <- ymd_hms(bike_rides$ended_at)

bike_rides <- bike_rides %>% mutate(Ride_length = seconds_to_period(ended_at-started_at))
bike_rides <- bike_rides %>% mutate(Day_of_week = wday(started_at))
bike_rides <- bike_rides %>% mutate(Month = month.abb[month(started_at)])
bike_rides <- bike_rides %>% mutate(Year = year(started_at))
bike_rides <- bike_rides %>% mutate(Ride_duration = (ended_at-started_at))

Min_Bike_Rides <- min(bike_rides$Ride_length)
Mean_Bike_Rides <- mean(bike_rides$Ride_length)
Max_Bike_Rides <- max(bike_rides$Ride_length)

bike_rides_result <- bike_rides %>% select(member_casual,Ride_length,Year) %>%
  group_by(member_casual,Year) %>% summarise(Avg_ride_length = mean(Ride_length))

bike_rides_result1 <- bike_rides %>% select(member_casual,Ride_length,Day_of_week) %>%
  group_by(member_casual,Day_of_week) %>% summarise(Number_of_rides = n())

bike_rides_result2 <- bike_rides %>% select(member_casual,Ride_length,Day_of_week) %>%
  group_by(Day_of_week) %>% summarise(Number_of_rides = n())

bike_rides_result3 <- bike_rides %>% select(member_casual,Ride_length,Month,Year,rideable_type) 

bike_rides_result4 <- bike_rides %>% select(Day_of_week,Ride_duration,member_casual) %>% 
group_by(Day_of_week,member_casual) %>% summarise(Trip_Duration = quantile(minute(seconds_to_period(Ride_duration)),
prob = c(0.15,0.25,0.5,0.75,0.85))) 

bike_rides_result <- bike_rides_result %>% pivot_wider(names_from  = Year, values_from = Avg_ride_length)
bike_rides_result1 <- bike_rides_result1 %>% pivot_wider(names_from  = Day_of_week, values_from = Number_of_rides)
bike_rides_result2 <- bike_rides_result2 %>% pivot_wider(names_from  = Day_of_week, values_from = Number_of_rides)
bike_rides_result2 <- sum(bike_rides_result2)

G1 <- ggplot(data = bike_rides)+ 
  geom_bar(mapping= aes(x= Year,fill=member_casual),position="dodge") +
  scale_x_continuous(breaks = c(2020,2021))+
  facet_wrap(~rideable_type)+
  labs(title="Cyclistic Bike Share" , subtitle= "Comparison between Casual and Member Riders Year-On-Year with different Rides", y = "Count of Rides")

G2 <- ggplot(data = bike_rides)+ 
     geom_bar(mapping= aes(x= Day_of_week,fill = factor(Day_of_week))) + theme(legend.position="none")+
  facet_wrap(~member_casual) + scale_x_continuous(breaks = c(1:7),labels = c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"))+
  labs(title="Cyclistic Bike Share" , subtitle= "Analyzing which Week-Day has maximum Rides", y = "Count of Rides")

G3 <- ggplot(data = bike_rides)+ 
  geom_bar(mapping= aes(x= Month,fill=member_casual)) +
   theme(legend.position = "top",axis.text.x = element_text(angle = 45))+ 
  labs(title="Cyclistic Bike Share" , subtitle= "Comparison between Casual and Member Riders Month-on-Month basis with different Rides type", y = "Count of Rides")+
  facet_wrap(~Year) 

G4 <- ggplot(data = bike_rides_result4)+ 
  geom_boxplot(mapping= aes(x= factor(Day_of_week), y=Trip_Duration, fill=member_casual),position="dodge") +
  theme(legend.position="bottom")+
  scale_x_discrete(breaks = c(1:7),labels = c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat")) +
  labs(title="Cyclistic Bike Share" , subtitle= "Trip Duration for each Ride on Week-Day basis comparing Casual Vs Annual members", x="Day_of_week" ,y = "Duration (mins)")


write.csv(bike_rides,"bike_rides.csv")