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

#types <- group_by(crime$, CATEGORY)
 # (summarize(total=n())



types
p<-ggplot(data=crime, aes(x=factor(types))) +
  geom_bar(stat="count")
p
class(crime$INCIDENTDATE)

