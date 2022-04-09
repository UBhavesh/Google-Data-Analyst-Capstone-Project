library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
library(scales)



rm(list = ls())
gc()

getwd
df1 <- read.csv("H:\\analytics\\google\\final csv in r\\202011-divvy-tripdata.csv") 
glimpse(df1)
df2 <- read.csv("H:\\analytics\\google\\final csv in r\\202012-divvy-tripdata.csv") 
df3 <- read.csv("H:\\analytics\\google\\final csv in r\\202101-divvy-tripdata.csv") 
df4 <- read.csv("H:\\analytics\\google\\final csv in r\\202102-divvy-tripdata.csv") 
df5 <- read.csv("H:\\analytics\\google\\final csv in r\\202103-divvy-tripdata.csv") 
df6 <- read.csv("H:\\analytics\\google\\final csv in r\\202104-divvy-tripdata.csv") 
df7 <- read.csv("H:\\analytics\\google\\final csv in r\\202105-divvy-tripdata.csv") 
df8 <- read.csv("H:\\analytics\\google\\final csv in r\\202106-divvy-tripdata.csv") 
df9 <- read.csv("H:\\analytics\\google\\final csv in r\\202107-divvy-tripdata.csv") 
df10 <- read.csv("H:\\analytics\\google\\final csv in r\\202108-divvy-tripdata.csv") 
df11 <- read.csv("H:\\analytics\\google\\final csv in r\\202109-divvy-tripdata.csv") 
df12 <- read.csv("H:\\analytics\\google\\final csv in r\\202110-divvy-tripdata.csv") 


gc()

bike_ride <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

save(bike_ride,file = "H:\\bike_ride.RData")
load("H:\\bike_ride.RData")


load('H:\\analytics\\google\\new\\bike_ride.RData')
bike_ride$started_at <- lubridate::ymd_hms(bike_ride$started_at)
bike_ride$ended_at <- lubridate::ymd_hms(bike_ride$ended_at)

bike_ride$started_at_date <- lubridate::ymd(bike_ride$started_at_date)
bike_ride$ended_at_date <- lubridate::ymd(bike_ride$ended_at_date)

bike_ride$started_at_time <- lubridate::hms(bike_ride$started_at_time)
bike_ride$ended_at_time <- lubridate::hms(bike_ride$ended_at_time)

bike_ride$ride_length <- lubridate::hms(bike_ride$ride_length)

#create hour field
bike_ride$start_hour <-  lubridate::hour(bike_ride$started_at_time)
bike_ride$end_hour <- lubridate::hour(bike_ride$ended_at_time)
bike_ride$start_hour <- as.integer(bike_ride$start_hour)
bike_ride$end_hour <- as.integer(bike_ride$end_hour)


glimpse(df1)
dim(bike_ride)





#
ggplot(data = bike_ride)+
  geom_bar(mapping=aes(x=bike_ride$member_casual),width = 0.3)+
  scale_y_continuous(labels = comma)+
  labs(title ="Count of member",x="Name of member",y="Count of member")


install.packages("rmarkdown")


bike_ride %>% 
  group_by(member_casual,day_of_week)%>%
  dplyr::summarise(no_of_rides = n())%>%
  arrange(member_casual,day_of_week)%>% 
  ggplot(aes(x=day_of_week,y=no_of_rides,fill=member_casual))+ geom_col(position = "dodge")+
  scale_y_continuous(labels = comma)+
  labs(title ="Count of ride by Day of Week",x="Day of week",y="Count of Ride")




ggplot(data = bike_ride)+
  geom_bar(mapping=aes(x=bike_ride$start_hour))+
  scale_y_continuous(labels = comma)+
  labs(title ="Count of ride by Hour in 12 months",x="Hour",y="Count of Ride")


bike_ride %>% 
  group_by(member_casual,rideable_type)%>%
  dplyr::summarise(no_of_rides = n())%>%
  arrange(member_casual,rideable_type)%>% 
  ggplot(aes(x=member_casual,y=no_of_rides,fill=rideable_type))+ 
  geom_col(position = "dodge")+
  labs(title ="Count of Bike Type used by casual and annual member",x="Membership type",y="Count of Rideable Type ")

summary(bike_ride)##


count(bike_ride$member_casual)
mean(bike_ride$ride_length)
max(bike_ride$ride_length)
min(bike_ride$ride_length)
median(bike_ride$ride_length)
summary(bike_ride$ride_length)

aggregate(bike_ride$ride_length ~ bike_ride$member_casual+bike_ride$day_of_week,FUN = mean )

n_distinct(bike_ride$start_station_name)

#Top Ten Start Stations for Casual Riders
bike_ride %>% 
  group_by(start_station_name)%>% 
  filter(member_casual == "casual")%>%
  dplyr::summarise(n = n())%>%
  arrange(desc(n)) %>%
  head(10)

#Top Ten Start Stations for member Riders
bike_ride %>% 
  group_by(start_station_name)%>% 
  filter(member_casual == "member")%>%
  dplyr::summarise(n = n())%>%
  arrange(desc(n)) %>%
  head(10)

#Use of bike by Hour of the Day
bike_ride%>%
  ggplot(aes(start_hour,fill=member_casual))+
  scale_y_continuous(labels = comma)+
  labs(x="Hour of the Day", title="Use of bike by Hour of the Day")+
  geom_bar()

#Use of bike by Hour of the Day divided by weekday
bike_ride%>%
  ggplot(aes(start_hour,fill=member_casual))+
  scale_y_continuous(labels = comma)+
  labs(x="Hour of the Day", title="Use of bike by Hour of the Day divided by weekday")+
  geom_bar()+
  facet_wrap(~ day_of_week)



