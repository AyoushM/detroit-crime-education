library(acs)
library(geo)
library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(reshape) 
library(data.table)
#all.tracts=geo.make(state="MI", county=26163,tract="*",table.number="B06011")
#my.tracts=geo.make(state="MI", county="Wayne", tract="*",table.number="B06011") 
#income <- acs.fetch(geography=my.tracts,key = "87ba1d142930b008a4f9ebe41cb5dcf2e43a6d68",endyear = 2015,table.number="B06011")

crime <- read.csv("Crime.csv")
crime <- data.table(crime)

#Removing  Unused columns
crime$STATEOFFENSEFILECLASS <- NULL
crime$CRNO <- NULL
crime$CRIMEID <- NULL
#crime$CASEID <- NULL
#Number of rows
nrow(crime)
ncol(crime)
# Creating new columns for month and year
#converting year to date format
crime$INCIDENTDATE <- as.Date(crime$INCIDENTDATE,format = "%m/%d/%Y")
#crime$INCIDENTDATE1<- NULL
crime$MONTH <- month(crime$INCIDENTDATE)
unique(crime$MONTH)



crime$YEAR <- year(crime$INCIDENTDATE)
unique(crime$YEAR)
#Histogram of total crimes across months
hist(crime$MONTH,main="Histogram of Crime per Month",xlab="Month",ylab="Number of Crimes",breaks = 11)
qplot(crime$MONTH, geom="histogram")+
  labs(x="Month",title="Crime Distribution across Months")
head(crime)
#Removing rows for year 2017
crime <- crime[!crime$YEAR==2017]

#aggregate(crime[,c("CATEGORY")],by=list("YEAR"),FUN=length)

#Crime by year and category
crimebyyear <- ddply(crime[,c("YEAR","CATEGORY")],.(crime$YEAR,crime$CATEGORY),nrow)
names(crimebyyear) <- c("YEAR","CATEGORY","COUNT")
crimebyyear <- data.table(crimebyyear)
crimebyyear
#Annual mean crime
annualcrime <- crimebyyear[,list(sum.freq=sum(COUNT)),by=list(YEAR)]
annualcrime
#Plot of annual mean crime
ggplot(data=annualcrime,aes(x=YEAR,y=sum.freq))+
  geom_bar(aes(fill=sum.freq),stat = "identity")+
  labs(title=" Annual Crimes in Wayne County", y="Total number of cases reported ",x="year")

crimebyyear
cbyyear <- cast(crimebyyear, YEAR~CATEGORY)
cbyyear
plot(cbyyear$ASSAULT)
plot(cbyyear$`AGGRAVATED ASSAULT`)

 crime[,list(total.crime=sum(CATEGORY)),by=list(YEAR,MONTH)]


#Grouping crime by category
category_crime <- crimebyyear[,list(total.crime=sum(COUNT)),by=list(CATEGORY)]
#Sort the category by count
order_category <- category_crime[order(category_crime$total.crime,decreasing = TRUE),]
total <- sum(order_category$total.crime[-1])
#Total crime excluding misc - 1027027
sum(order_category$total.crime[2:26])/total



refined_category <- data.table(order_category[2:11])
refined_category
ggplot(refined_category,aes(x=CATEGORY,y=total.crime))+
  geom_bar(aes(fill=CATEGORY),stat="identity")+
  labs(title="Top 10 most frequent Crime Categories",y="#Crimes Reported(09-16)",x="Category" )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

unique(crime$CATEGORY)
write.csv(unique(crime$OFFENSEDESCRIPTION),file="offense.csv")

#crime by description
offensebyyear <- ddply(crime[,c("YEAR","OFFENSEDESCRIPTION")],.(crime$OFFENSEDESCRIPTION),nrow)
offensebyyear
write.csv(offensebyyear,"offensebyyear.csv")
neighborhoods <- unique(crime$NEIGHBORHOOD)
head(offensebyyear)
#Organize crime by neighborhood, year and category
crimebyneighbor <- ddply(crime[,c("YEAR","MONTH","NEIGHBORHOOD","CATEGORY")],.(crime$YEAR,crime$MONTH,crime$NEIGHBORHOOD,crime$CATEGORY),nrow)
#crimebyneighbor <- ddply(crime[,c("YEAR","NEIGHBORHOOD")],.(crime$NEIGHBORHOOD,crime$YEAR),nrow)
names(crimebyneighbor) <- c("YEAR","MONTH","NEIGHBORHOOD","CATEGORY","COUNT")
crimebyneighbor<- data.table(crimebyneighbor)
crimebyneighbor

crimetime <- crimebyneighbor[,TOTAL:= sum(COUNT),by=list(MONTH,YEAR)]

neighbcast <- cast(crimebyneighbor,CATEGORY~YEAR,count)
neighbcast
#arrange(neighbcast,"2009")
neighbcast[,"TOTAL"] <- rowSums(neighbcast[,3:10])

neighbcast
neighbcast[order(neighbcast$TOTAL,decreasing = TRUE),]
l2<- ll[1:10,c("TOTAL")]
plot(neighbcast$NEIGHBORHOOD,neighbcast$TOTAL)
filter()
plot(ll$NEIGHBORHOOD,ll$TOTAL)


library(dplyr)
library(tidyr)


#Extract Coordinates from LOCATION
#gsub(".*\\((.*)\\).*", "\\1", crime$LOCATION)
#crime$COORD <- gsub(".*\\((.*)\\).*", "\\1", crime$LOCATION)
crime$LAT <- round(crime$LAT,digits = 3)
crime$LON <- round(crime$LON,digits = 3)
crime$ROUNDLAT <- round(crime$LAT,digits = 0)
crime$ROUNDLON <- round(crime$LON,digits = 0)

#Creating a coordinate column
crime$COORD <- paste(round(crime$LAT,digits = 3),",",round(crime$LON,digits = 3))

#Sorting the dataset by lat and long
crime[with(crime,order(ROUNDLAT,ROUNDLON)),]

crime <- data.table(read.csv("cleanedcrime.csv"))
#Removing junk latitudes and longitudes

crime <- crime[ROUNDLAT !=0 | ROUNDLON !=0]
crime <- crime[ROUNDLAT < 91 | ROUNDLON <180]
library(zipcode)

#Add Zipcodes
data(zipcode)

zips <- subset(zipcode, city=='Detroit'&state=='MI')
#subset(crime, crime$NEIGHBORHOOD)
crime[NEIGHBORHOOD %like% 'MASONIC TEMPLE']$ZIPCODES=48201
neighbfreq <- table(crime$NEIGHBORHOOD)
ord <- order(neighbfreq)
neighbfreq[ord]

#Frequency plot of neighborhoods
plot(neighbfreq[ord])

zips$LAT <- round(zips$latitude,digits = 3)
zips$LON <- round(zips$longitude,digits=3)

locs <- crime[,c("LAT","LON")]
locs[with(locs,order)]

#Prinr the Unique coordinates in data
unique(locs[, c("LAT","LON")])
crime[crime$LAT %in% (42.34:42.35) & crime$LON %in% (-83.06:-83.05) ]
for(i in 1:25){
  lat <- zips[i,"LAT"]
  lon <- zips[i,"LON"]
  cat(lat,lon," ")
  cat(signif(lat,4), " " ,round(lat,digits = 2))
  cat(signif(lon,4), " ", round(lon,digits = 2))
  matchrows <- crime[crime$LAT %in% (signif(lat,4):round(lat,digits = 2)) & 
                 crime$LON %in% (signif(lon,4):round(lon,digits = 2)) ]
  
  print(dim(matchrows)[1])
}
  
  
#Map Exploration
library(maptools)
library(rgeos)


library(ggmap)
library(ggplot2)
library(zipcode)
library(choroplethrMaps)
library(choroplethr)
# Key for Google Map API: AIzaSyDMYl2C5Kq-n9Kad1vAF91qhWIZ1WLINmE
#Fetch unique coordinates
locs <- unique(locs[, c("LAT","LON")])
#Add a zipcode column
locs$ZIPCODE <-1 
locs$ZIPCODE <- as.factor(locs$ZIPCODE)
#locs$COORD <- paste(locs$LON, locs$LAT)
#res <- revgeocode(code,output = "more")
#locs$CODE <- as.numeric(geocode(locs$COORD))
#store <- list()
#2415 missing
for(i in 2484:2500){
  # Create a coordinate vector for api parameter
  code <- c(locs[i,LON],locs[i,LAT])
  #Reverse code using Google API
  res <- revgeocode(code,output = "more")
  store[[i]] <- res
  locs[i,"ZIPCODE"] <- res$postal_code
}

write.csv(locs,"zipcodes.csv")
#locs[1,"ZIPCODE"] <- store[[1]]$postal_code

#for(j in 11:2){
#  locs[j,"ZIPCODE"] <- store[[j]]$postal_code
#}

#Google API Limits so using Opencage for another 2500
OPENCAGE_KEY <- "b6a9b4fbbf4ecbae385d6edc0d489e67"
library("opencage")

for(i in 4951:5000){
  #Reverse code using OpenCage API
  res2 <- opencage_reverse(latitude = locs[i,LAT],longitude = locs[i,LON],key=OPENCAGE_KEY)

  locs[i,"ZIPCODE"] <- res2$results[,"components.postcode"]
}
devtools::install_github("hrbrmstr/nominatim")
library(nominatim)
#Writing to csv
write.csv(locs,"zipcodes.csv")

library(nominatim)

#NOMINATOM! This is it i think...15k per day.
NOMINATO_KEY <- "YPbw5WTRnAWeh8ISvEL0hRyayr9UmHnC"
#rgb <- reverse_geocode_coords(locs[1,LAT], locs[1,LON],key = NOMINATO_KEY )

for(i in 36523:36658){
  rg <- reverse_geocode_coords(locs[i,LAT], locs[i,LON],key = NOMINATO_KEY )
  cat(i)
  locs[i,"ZIPCODE"] <- as.factor(rg$postcode)
}
write.csv(locs,"zipcodes2.csv")
#locs1:ZipCode2:1-21999, 35000-36534
#ZipCode3:22000-33414
locs1<- data.table(read.csv("zipcodes2.csv"))
locs2<- data.table(read.csv("zipcodes3.csv"))
locs_final <- rbind(locs1[1:21999],locs2[22000:34999],locs1[35000:36658])
#Issue with 7940
#11481
#12969,12970

crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
states=readShapePoly("./ShapeFiles/tl_2013_26163_roads.shp",proj4string=crswgs84,verbose=TRUE)

area <- readShapePoly("./ShapeFiles/tl_2013_26163_roads.shp")

states_map <- map_data("county")
head(states_map)
county_choropleth(zips, title = "California County Vote Percentage", legend = "Vote", state_zoom = "california", reference_map = TRUE)


data$zip = clean.zipcodes(data$ZIP)
data(zipcode)
data=merge(data,zipcode,by.x="zip",by.y="zip",)
View(data)
ggmap(get_map("Wayne County, Michigan",zoom=10,color = "bw")) 
#Choloropeth for population
data(county.regions)
data(df_pop_county)
county_choropleth(df_pop_county,
                  title   = "2012 Population Estimates",
                  legend  = "Population",
                  state_zoom = "michigan",
                  num_colors = 1)


