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

# Streetmap Plot
ggmap(Map.Wayne.County) + geom_tile(data = school.frequency,
                                          aes(x = Longitude, y = Latitude, alpha = Freq),
                                          fill = 'blue') + geom_point(data = School.Data, show.legend = TRUE, aes(x = Longitude, y = Latitude), col = 'red')


# Toner Plot
ggmap(Map.Wayne.County.Toner) + geom_tile(data = school.frequency,
                                          aes(x = Longitude, y = Latitude, alpha = Freq),
                                          fill = 'blue') + geom_point(data = School.Data, show.legend = TRUE, aes(x = Longitude, y = Latitude), col = 'red')


# Graduation Gradient Plot
grad.grad.data = subset(Edu.Data, select = c(Total.Cohort, Total.Graduated, OffTrack.Other, Dropouts, gradyear, enrollyear, ZIP.Code, Longitude, Latitude))
grad.grad.data$Longitude = round(grad.grad.data$Longitude, 2)
grad.grad.data$Latitude = round(grad.grad.data$Latitude, 2)

# Aggregation of the data
# By Year
grad.grad.data.by_year = aggregate(grad.grad.data, by = list(grad.grad.data$ZIP.Code, grad.grad.data$Longitude, grad.grad.data$Latitude, grad.grad.data$gradyear), FUN = sum)
grad.grad.data.by_year = grad.grad.data.by_year[, -c(7,9,10,11,12,13)]
grad.grad.data.by_year$gradrate = grad.grad.data.by_year$Total.Graduated/grad.grad.data.by_year$Total.Cohort
grad.grad.data.by_year$droprate = grad.grad.data.by_year$Dropouts/grad.grad.data.by_year$Total.Cohort
grad.grad.data.by_year = grad.grad.data.by_year[, -c(5,6,7)]
names(grad.grad.data.by_year) = c("ZIP.Code", "Longitude", "Latitude", "GradYear", "GradRate", "DropRate")

# Overall
grad.grad.data.overall = aggregate(grad.grad.data, by = list(grad.grad.data$ZIP.Code, grad.grad.data$Longitude, grad.grad.data$Latitude), FUN = sum)
grad.grad.data.overall = grad.grad.data.overall[, -c(6,8,9,10,11,12)]
grad.grad.data.overall$gradrate = grad.grad.data.overall$Total.Graduated/grad.grad.data.overall$Total.Cohort
grad.grad.data.overall$droprate = grad.grad.data.overall$Dropouts/grad.grad.data.overall$Total.Cohort
grad.grad.data.overall = grad.grad.data.overall[, -c(4,5,6)]
names(grad.grad.data.overall) = c("ZIP.Code", "Longitude", "Latitude", "GradRate", "DropRate")

#Plot of Overall Graduation Rate
ggmap(Map.Wayne.County.Toner) + geom_tile(data = grad.grad.data.overall,
                                          aes(x = Longitude, y = Latitude, fill = GradRate)) + scale_fill_gradientn(colors = c("Yellow", "Green", "DarkBlue"), breaks = c(0, 0.33, 0.66, 1))
