library(RPostgreSQL)
library(sqldf)
library(dplyr)
library(sp)
library(rgdal)

residentList <- readRDS("/home/cynthiak/residents.Rds")

###Read shapefile 
us.map <- readOGR("/home/cynthiak/county", "tl_2018_us_county", stringsAsFactors = FALSE)
# Remove Alaska(2), Hawaii(15), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]

###Postgresql
drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='cynthiak',password='Kamalendra1985')

#Getting all the tweets with geocoded information
allTweetsDuringQ3<-dbGetQuery(conn,
"SELECT 
 CAST(tweetuser AS varchar(1000)),
 date(tweetcreated),
 TRIM (LEADING '[' 
 FROM split_part(tweetgeocoord, ',', 1)
 ) as lat, 
 TRIM (TRAILING ']'
 FROM split_part(tweetgeocoord, ',', 2)
 ) as long,
 tweetpname,
 COUNT(tweettext)
FROM tweets_info 
WHERE tweetgeotype='Point'
AND tweetcreated BETWEEN '2017-08-09' AND '2017-08-12'
GROUP BY  tweetuser,date(tweetcreated),lat,long,tweetpname
ORDER BY date(tweetcreated);
")

residentTweetsQ3<- allTweetsDuringQ3[allTweetsDuringQ3$tweetuser %in% residentList,]


##GeoCode to County map
for(i in 1:nrow(residentTweetsQ3)) {
  Lat3 <- as.double(residentTweetsQ3[i,"lat"]) 
  Lon3 <- as.double(residentTweetsQ3[i,"long"])
  #make a data frame
  coords3 <- as.data.frame(cbind(Lon3,Lat3))
  #and into Spatial
  points3 <- SpatialPoints(coords3)
  #assume same proj as shapefile!
  proj4string(points3) <- proj4string(us.map)
  #get county polygon point is in
  residentTweetsQ3[i, "countyFIPS"] <- as.character(over(points3, us.map)$GEOID)
}

#### Save ResidentTweetsQ3 for fututre use ####

saveRDS(residentTweetsQ3, "residentTweetsQ3.Rds")
#residentTweetsQ3 <- readRDS("residentTweetsQ3.Rds")

##Delete null counties
residentTweetsQ3 <- residentTweetsQ3[!(is.na(residentTweetsQ3$countyFIPS) | residentTweetsQ3$countyFIPS==""), ]

##No. of tweets per county - First let's groupby username and countyid
#and then sum up the users for each county
tweetCountPerCountyQ3 <- residentTweetsQ3 %>%
  group_by(tweetuser,countyFIPS) %>%
  summarize(n())
tweetCountPerCountyQ3 <- aggregate(tweetCountPerCountyQ3$`n()`, by=list(GEOID=tweetCountPerCountyQ3$countyFIPS), FUN=sum)

############ Visualization ###############

#Rename countyfips column name to match shapefile
colnames(tweetCountPerCountyQ3) <- c("GEOID", "count")

tweetCountPerCountyQ3$GEOID <- formatC(tweetCountPerCountyQ3$GEOID, width = 5, format = "d", flag = "0")

#Merge shapefile with data file
leafmap <- merge(us.map, tweetCountPerCountyQ3, by=c("GEOID"))
leafmap[is.na(leafmap$count)] <- 0

leafmap <- leafmap[leafmap$count >5,]

#Define popup info
popup_dat <- paste0("<strong>County: </strong>", 
                    leafmap$NAME, 
                    "<br><strong>Tweet User Count: </strong>", 
                    leafmap$count)

#Define color pallete
pal <- colorNumeric("RdYlBu", domain=5:max(leafmap$count))

#Print resident tweet count per county in the selected counties
leaflet(data = leafmap) %>% addTiles() %>%
  addPolygons(fillColor = ~pal(count), 
              stroke = TRUE,
              color = "blue",
              opacity = 5,
              fillOpacity = 5, 
              weight = 0.2, 
              smoothFactor = 0.2,  
              popup = popup_dat)
