# Acquiring the data
setwd("/Users/ayoushmukherjee/Documents/INFX 573/Group Project/New Datasets")

d1 = read.csv("Clean/2008-09 Education Data.csv")
d2 = read.csv("Clean/2009-10 Education Data.csv")
d3 = read.csv("Clean/2010-11 Education Data.csv")
d4 = read.csv("Clean/2011-12 Education Data.csv")
d5 = read.csv("Clean/2012-13 Education Data.csv")
d6 = read.csv("Clean/2013-14 Education Data.csv")
d7 = read.csv("Clean/2014-15 Education Data.csv")

# Data Correection
d1$gradyear = 2009
d2$gradyear = 2010
d3$gradyear = 2011
d4$gradyear = 2012
d5$gradyear = 2013
d6$gradyear = 2014
d7$gradyear = 2015

# Data Aggregation
data = rbind(d1, d2, d3, d4, d5, d6, d7)
data$enrollyear = data$Cohort.Graduation.Year - 4

# Sorting
data[order(data$Location.Name, data$enrollyear), ]

# Unique School Names
school.names = unique(data$Location.Name)
school.names

# Write to CSVs
write.csv(data, "Clean/Aggregate Data.csv")
write.csv(school.names, "Clean/School names.csv")

# Summary of the data
str(data)
summary(data)

# Plots
agg.data = aggregate(data, by = list(data$Cohort.Graduation.Year), FUN = mean, na.rm = TRUE)
library(ggplot2)

# Dropouts
subset(agg.data, select = c("Cohort.Graduation.Year", "Dropout.Rate"))
ggplot(agg.data,aes(x=agg.data$gradyear,y=agg.data$Dropout.Rate))+
  geom_point()+geom_line()+
  labs(x="Year",y="Cohort Dropout Rate", title="Dropout Rate v/s Year")

# Graduates
subset(agg.data, select = c("Cohort.Graduation.Year", "Graduation.Rate"))
ggplot(agg.data,aes(x=agg.data$gradyear,y=agg.data$Graduation.Rate))+
  geom_point()+geom_line()+
  labs(x="Year",y="Cohort Graduation Rate", title="Graduation Rate v/s Year")
