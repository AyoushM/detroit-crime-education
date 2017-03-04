# Education Heatmap Trial
library(ggplot2)
library(ggmap)

# Importing Datasets
Edu.Data = read.csv("data/clean/Education/Education_Data_wLoc.csv")
School.Data = read.csv("data/clean/Education/School Data.csv")

# Fetching Map Data
# StreetMap
Map.Wayne.County = get_map(location = c(lon = -83.178249, lat = 42.267666),
                           zoom = 10)
ggmap(Map.Wayne.County,
      extent = "device")

# TonerMap
Map.Wayne.County.Toner = get_map(location = c(lon = -83.178249, lat = 42.267666),
                                 zoom = 10,
                                 maptype = 'toner')
ggmap(Map.Wayne.County.Toner,
      extent = "device")

# Plot of Schools
ggmap(Map.Wayne.County) + geom_point(data = School.Data, show.legend = TRUE,
                                     aes(x = Longitude, y = Latitude), col = 'red')

ggmap(Map.Wayne.County.Toner) + geom_point(data = School.Data, show.legend = TRUE,
                                     aes(x = Longitude, y = Latitude), col = 'red')


# Heatmap - According to Lat & Long
school.freq = data.frame(list(School.Data$Longitude, School.Data$Latitude))
names(school.freq) = c("Longitude", "Latitude")
school.freq$Longitude = round(school.freq$Longitude, 1)
school.freq$Latitude = round(school.freq$Latitude, 1)

school.frequency = as.data.frame(table(school.freq$Longitude, school.freq$Latitude))
names(school.frequency) = c("Longitude", "Latitude", "Freq")
# Removing all zero frequencies
# school.frequency = subset(school.frequency, Freq > 0)
school.frequency$Longitude = as.numeric(as.character(school.frequency$Longitude))
school.frequency$Latitude = as.numeric(as.character(school.frequency$Latitude))

# Strretmap Plot
ggmap(Map.Wayne.County) + geom_tile(data = school.frequency,
                                          aes(x = Longitude, y = Latitude, alpha = Freq),
                                          fill = 'blue') + geom_point(data = School.Data, show.legend = TRUE, aes(x = Longitude, y = Latitude), col = 'red')


# Toner Plot
ggmap(Map.Wayne.County.Toner) + geom_tile(data = school.frequency,
                                          aes(x = Longitude, y = Latitude, alpha = Freq),
                                          fill = 'blue') + geom_point(data = School.Data, show.legend = TRUE, aes(x = Longitude, y = Latitude), col = 'red')

