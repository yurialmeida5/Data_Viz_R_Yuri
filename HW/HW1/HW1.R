# HW1 - Yuri Almeida Cunha

# Load the nycflights13 package and check what kind of datasets exist in the package, 
# then create a copy of flights dataset into a data.table object, called flight_data

library(data.table)
library(dplyr)
library(ggplot2)
library(tsibble)
require(maps)

#install.packages("nycflights13")
library(nycflights13)
#check what kind of datasets exist in the package
browseURL("https://www.rdocumentation.org/packages/nycflights13")
flight_data <- data.table(nycflights13::flights)

# Which destination had the lowest avg arrival delay from LGA with minimum 100 flight to that destination?
flight_data[origin == 'LGA',list(mean_arr_delay = mean(arr_delay , na.rm = TRUE), number_of_flights = .N ),
            by = dest][number_of_flights > 100][order(mean_arr_delay)][1]

# Which destinations flights were the most on time (avg arrival delay closest to zero) from LGA with minimum 100 flight to that destination?
flight_data[origin == 'LGA',list(mean_arr_delay = mean(arr_delay , na.rm = TRUE), number_of_flights = .N ),
            by = dest][number_of_flights > 100 & mean_arr_delay < 0][order(mean_arr_delay , decreasing = TRUE)][1]

# Who is the manufacturer of the plane, which flights the most to CHS destination?
flight_data <- data.table(merge(nycflights13::flights, nycflights13::planes , by = 'tailnum'))
flight_data[dest == 'CHS' & tailnum == 'N713EV', list(number_of_flights = .N) , by = list(manufacturer,tailnum)]

# Who is the manufacturer of the plane, which flights the most to CHS destination?
flight_data <- data.table(merge(nycflights13::flights, nycflights13::airlines , by = 'carrier'))
flight_data[ , list(sum_of_distance = sum(distance)), by = list(name)][order(sum_of_distance, decreasing = TRUE)][1]

# Plot the monthly number of flights with 20+ mins arrival delay!
flight_data <- data.table(nycflights13::flights)
ggplot(flight_data[ , year_month:= yearmonth(time_hour)][arr_delay > 20, .N , year_month],
       aes(x = year_month, y = N)) + geom_bar(stat = 'identity') +
       xlab('Date') + ylab('number_of_flights')

# Plot the departure delay of flights going to IAH and the related day’s wind speed on a scaterplot! Is there any association between the two variables? Try adding a linear model.
weather <- data.table(nycflights13::weather)[ , key_col:= paste0(year ,month, day)][,list(avg_wind_speed = mean(wind_speed, na.rm = TRUE)), by = key_col]
flights <- data.table(nycflights13::flights)[ , key_col:= paste0(year ,month, day)][dest == 'IAH']
flight_data <- merge(flights, weather , by = 'key_col')

ggplot(flight_data, aes(dep_delay, avg_wind_speed)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  xlab('Departure Delay') + ylab('Mean of wind speed') +
  ylim(c(0,60))
  
# Plot the airports as per their geolocation on a world map, by mapping the number flights going to that destionation to the size of the symbol!

flight_data <- data.table(merge(
  nycflights13::flights, nycflights13::airports, by.x = 'dest', by.y = 'faa'
)) [ , list(lat = mean(lat), lon = mean(lon), flights = .N ), by = dest ]

world_map = map_data('world')
 ggplot(data = world_map) +
  geom_polygon(mapping=aes(x=long,y=lat,group=group),color="white",fill="grey") +
  geom_point(data = flight_data, mapping = aes(x = lon , y = lat , size = flights)) +
   theme(legend.position = "top")
            

