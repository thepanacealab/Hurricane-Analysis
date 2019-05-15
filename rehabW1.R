library(RPostgreSQL)
library(sqldf)
library(dplyr)
library(sp)
library(rgdal)

residentList <- readRDS("/home/cynthiak/residents.Rds")

#Read shapefile 
us.map <- readOGR("/home/cynthiak/county", "tl_2018_us_county", stringsAsFactors = FALSE)

drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='####',password='####')

#Getting all the tweets with geocoded information
allTweetsAfterW1<-dbGetQuery(conn,
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
AND tweetcreated BETWEEN '2017-08-17' AND '2017-08-23'
GROUP BY  tweetuser,date(tweetcreated),lat,long,tweetpname
ORDER BY date(tweetcreated);
")

residentTweetsW1<- allTweetsAfterW1[allTweetsAfterW1$tweetuser %in% residentList,]

##GeoCode to County map
for(i in 1:nrow(residentTweetsW1)) {
  Lat11 <- as.double(residentTweetsW1[i,"lat"]) 
  Lon11 <- as.double(residentTweetsW1[i,"long"])
  #make a data frame
  coords11 <- as.data.frame(cbind(Lon11,Lat11))
  #and into Spatial
  points11 <- SpatialPoints(coords11)
  #assume same proj as shapefile!
  proj4string(points11) <- proj4string(us.map)
  #get county polygon point is in
  residentTweetsW1[i, "GEOID"] <- as.character(over(points11, us.map)$GEOID)
}

#### Save ResidentTweetsW1 for fututre use ####

saveRDS(residentTweetsW1, "residentTweetsW1.Rds")
#residentTweetsW1 <- readRDS("residentTweetsW1.Rds")

##Delete null counties
residentTweetsW1 <- residentTweetsW1[!(is.na(residentTweetsW1$GEOID) | residentTweetsW1$GEOID==""), ]

##No. of tweets per county
tweetCountPerCountyW1 <- residentTweetsW1 %>%
  group_by(tweetuser,GEOID) %>%
  summarize(n())
tweetCountPerCountyW1
tweetCountPerCountyW1 <- aggregate(tweetCountPerCountyW1$`n()`, by=list(GEOID=tweetCountPerCountyW1$GEOID), FUN=sum)

### Visualization ###

#Rename countyfips column name to match shapefile
colnames(tweetCountPerCountyW1) <- c("GEOID", "count")

tweetCountPerCountyW1$GEOID <- formatC(tweetCountPerCountyW1$GEOID, width = 5, format = "d", flag = "0")

#Merge shapefile with data file
leafmap <- merge(us.map, tweetCountPerCountyW1, by=c("GEOID"))
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
