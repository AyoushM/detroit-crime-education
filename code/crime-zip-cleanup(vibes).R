library(acs)
library(geo)
library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(reshape) 
library(data.table)

#Michigan has Zipcodes of the format 4xxxx
#Let's clean our results from the Reverse Geocoding Process 

#ZIP CODES FOLDER LOCATION: detroit-crime-education/data/clean/crime
zips <- read.csv("zipcodes_final.csv")

#Summary of data
summary(zips)
head(zips)

#Create a ZIP character column from ZIPCODE Factor Column (to be able to compare and filter)
zips$ZIP = as.character(zips$ZIPCODE)
nrow(zips)

#There are many zips with code less than 40,000!
badzips = subset(zips, zips$ZIP<40000)
badzips
dim(badzips)
#87 such incidents with bad location!

#The rest of the zips which have zip code > 40,000
goodzips= subset(zips, zips$ZIP>=40000)
goodzips
nrow(goodzips)
dim(goodzips)
#36571 rows with good zip data possibly good zip codes :)

#Let's filter out the zip codes from the 40K+ ones that are weird!
# There are multiple values in single cell e.g. "48221:48235;48235"
multi.goodzips= goodzips[grep(":",goodzips$ZIP),]
multi.goodzips
dim(multi.goodzips)
#267 rows with multiple possible zip values! Need to decide....


#Now, we need to create a "Final Zip" Value from these multiple possibel zip values
# 
# Google API can help do this better.
# Let's use the Latitude and Longtitude and re-do the reverse geocode for just these rows.

#Create column to store final zip value, as a factor
multi.goodzips$FINALZIP <-1 
multi.goodzips$FINALZIP <- as.factor(multi.goodzips$FINALZIP)

#Convert multi.goodzips to a data table, to be able use reverse geocode
multi.goodzips <- data.table(multi.goodzips)

#Needed for Reverse Geocoding
library(ggmap)


#Code just is the latitude and longtitude in a vector format, needed for geocode input
code <- c(multi.goodzips[1,LON],multi.goodzips[1,LAT])
#-83.021  42.388

#Reverse geocode 1st row alone using Google API
res <- revgeocode(code, output = "more")
multi.goodzips[1,"FINALZIP"] <- res$postal_code


#What is this output from reverse geocode? we need the postal_code column
res
#    address                                street_number     route       locality    administrative_area_level_2  administrative_area_level_1       country postal_code
#1 8008 Harper Ave, Detroit, MI 48213, USA     8008        Harper Avenue  Detroit                Wayne County                    Michigan United States       48213


#Let's check what happened, after we pasted the postal code?
multi.goodzips

#We can see the 1st row for FinalZip has been changed to a single value from the multiple zip codes... Yaaaay!!
#       X.1    X    LAT     LON     ZIPCODE     ZIP       FINALZIP
#  1:  2611  2611 42.388 -83.021 48213:48224 48213:48224    48213
#  2:  3200  3200 42.387 -83.217 48223:48227 48223:48227        1
#  3:  4489  4489 42.444 -83.223 48075:48235 48075:48235        1
#  4:  4566  4566 42.439 -83.078 48211:48234 48211:48234        1
#  5:  4841  4841 42.439 -83.077 48211:48234 48211:48234        1
#---                                                            
#263: 35651 35651 42.354 -83.106 48204:48210 48204:48210        1
#264: 35897 35897 42.352 -83.101 48204:48210 48204:48210        1
#265: 35967 35967 42.477 -83.028 48021:48091 48021:48091        1
#266: 36314 36314 42.388 -83.042 48211:48234 48211:48234        1
#267: 36560 36560 42.448 -83.258 48025:48219 48025:48219        1


#Since there are 267 multiple valued rows of zips, let's work them all this way, starting from 2nd row
for(i in 2:8){
  # Create a coordinate vector for api parameter
  code <- c(multi.goodzips[i,LON],multi.goodzips[i,LAT])
  #Reverse code using Google API
  res <- revgeocode(code,output = "more")
  cat("Reverse Geocoded Row Number " , i)
  multi.goodzips[i,"FINALZIP"] <- res$postal_code
}
for(i in 10:240){
  # Create a coordinate vector for api parameter
  code <- c(multi.goodzips[i,LON],multi.goodzips[i,LAT])
  #Reverse code using Google API
  res <- revgeocode(code,output = "more")
  cat("Reverse Geocoded Row Number " , i)
  multi.goodzips[i,"FINALZIP"] <- res$postal_code
}
for(i in 242:267){
  # Create a coordinate vector for api parameter
  code <- c(multi.goodzips[i,LON],multi.goodzips[i,LAT])
  #Reverse code using Google API
  res <- revgeocode(code,output = "more")
  cat("Reverse Geocoded Row Number " , i)
  multi.goodzips[i,"FINALZIP"] <- res$postal_code
}

multi.goodzips[9]$ZIPCODE
multi.goodzips[241]$ZIPCODE
#Error at 9th Row and 241st with a "null" value


#Let's add actual values for rows 9, and 241 manually!!
multi.goodzips[9,]
#Lat = 42.43  Lon= -83.219
#Border of 48235 E, 48219 W 

multi.goodzips[241,]
#Lat = 42.378 Lon= -83.231
#48223 NE, 48228 SW

#####################
#Doing the same analysis for the bad zips!
#Need to replace all the zips for this, not just the multiple entry ones

badzips
dim(badzips)
#87 rows

#Create column to store final zip value, as a factor
badzips$FINALZIP <-1 
badzips$FINALZIP <- as.factor(badzips$FINALZIP)

#Convert multi.goodzips to a data table, to be able use reverse geocode
badzips <- data.table(badzips)


#Testing on 1st row of badzips!!
code <- c(badzips[77,LON],badzips[77,LAT])
code
#-82.964  42.404

#Reverse geocode 1st row alone using Google API
res <- revgeocode(code, output = "more")
badzips[1,"FINALZIP"] <- res$postal_code
badzips[1,]


#Row Numbers 1, 76 are confusing
# 87 is creating a problem. It is not a correct location!
for(i in 2:75){
  # Create a coordinate vector for api parameter
  code <- c(badzips[i,LON],badzips[i,LAT])
  #Reverse code using Google API
  cat("Reverse Geocoding for... Row Number : " ,i)
  res <- revgeocode(code,output = "more")
  badzips[i,"FINALZIP"] <- res$postal_code
}

for(i in 77:87){
  # Create a coordinate vector for api parameter
  code <- c(badzips[i,LON],badzips[i,LAT])
  #Reverse code using Google API
  cat("Reverse Geocoding for... Row Number : " ,i)
  res <- revgeocode(code,output = "more")
  badzips[i,"FINALZIP"] <- res$postal_code
}


#Row number 1 and 77 are in CANADA!! :D
badzips[1,]
badzips[77,]


#address street_number               route
#1 2696 Jefferson Blvd, Windsor, ON N8T 3C7, Canada          2696 Jefferson Boulevard
#neighborhood locality administrative_area_level_2 administrative_area_level_1 country
#1 Forest Glade  Windsor                Essex County                     Ontario  Canada
#postal_code
#1     N8T 3C
#################################################################################################################################

#Next Steps

#     Merging the final zip codes to incident data
#     Creating 

head(goodzips)
head(multi.goodzips) #Final Zip Column
head(badzips)        #Final Zip Column


#Remove columns
dim(goodzips)
multi.goodzips= goodzips[grep(":",goodzips$ZIP),]

write.csv(goodzips,"goodzips.csv")
write.csv(badzips, "badzips.csv")
write.csv(multi.goodzips, "multigoodzips.csv")

ids = unique(multi.goodzips$X)

goodzips = goodzips[!(goodzips$X%in%ids) ,]
nrow(goodzips)              #36304
nrow(multi.goodzips)        #  267 
length(ids) 
nrow(badzips)               #   87
nrow(zips)                  #36658

#Need a common FinalZip Column in the goodzips data table
goodzips$FINALZIP= goodzips$ZIPCODE
head(goodzips)

class(goodzips$FINALZIP)       #Factor
class(badzips$FINALZIP)        #Factor
class(multi.goodzips$FINALZIP) #Factor


#Binding together:  goodzips, badzips, multi.goodzips
zip_code_lookup = data.table()
zip_code_lookup = rbind(goodzips, multi.goodzips)
zip_code_lookup = rbind(zip_code_lookup, badzips)

#Write temporary file
write.csv(zip_code_lookup,"lookuptemp.csv")

#Clean up zip_code_lookup
zip_code_lookup = zip_code_lookup[,c("X","LAT","LON","FINALZIP", "ZIP")]
write.csv(zip_code_lookup,"ziplookup.csv")
