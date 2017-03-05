# Education Heatmaps - Actual

# Libraries
library(ggplot2)
library(ggmap)

# Importing Datasets
Edu.Data = read.csv("data/clean/Education/Education_Data_wLoc.csv")
School.Data = read.csv("data/clean/Education/School Data.csv")

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

# Fetching Map Data
# TonerMap
Map.Wayne.County.Toner = get_map(location = c(lon = -83.178249, lat = 42.267666),
                                 zoom = 10,
                                 maptype = 'toner')
ggmap(Map.Wayne.County.Toner, extent = "device")


# Plot of Overall Graduation Rate
ggmap(Map.Wayne.County.Toner) + geom_tile(data = grad.grad.data.overall,
                                          aes(x = Longitude, y = Latitude, fill = GradRate)) + scale_fill_gradientn(colors = c("Yellow", "Green", "DarkBlue"), breaks = c(0, 0.33, 0.66, 1)) + ggtitle("Overall Graduation Rate for Wayne County: 2009 - 2015") #+ annotate("text", x = -82.8, y = 42.1, label = "Overall")

ggsave("Education - Graduation Rate - Overall Year Plot.png", plot = last_plot(), device = "png")

# Plot of Year-on-year Graduation Rate
for(i in 2009:2015) {
ggmap(Map.Wayne.County.Toner) + geom_tile(data = grad.grad.data.by_year[grad.grad.data.by_year$GradYear == i,],
                                          aes(x = Longitude, y = Latitude, fill = GradRate)) + scale_fill_gradientn(colors = c("Yellow", "Green", "DarkBlue"), breaks = c(0, 0.33, 0.66, 1)) + ggtitle(paste0("Graduation Rate for Wayne County: ", i, collapse = NULL))
ggsave(paste0("Education - Graduation Rate - ", i, " Plot.png", collapse = NULL), plot = last_plot(), device = "png")
}
