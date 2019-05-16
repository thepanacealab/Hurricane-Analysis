library(RPostgreSQL)
library(sqldf)
library(dplyr)
library(sp)
library(rgdal)

residentList <- readRDS("/home/cynthiak/residentsMaria.Rds")

#Read shapefile 
us.map <- readOGR("/home/cynthiak/county", "tl_2018_us_county", stringsAsFactors = FALSE)

drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='####',password='####')

#Getting all the tweets with geocoded information
allTweetsDuringQ2<-dbGetQuery(conn,
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
AND tweetcreated BETWEEN '2017-09-21' AND '2017-09-24'
GROUP BY  tweetuser,date(tweetcreated),lat,long,tweetpname
ORDER BY date(tweetcreated);
")

residentTweetsQ2<- allTweetsDuringQ2[allTweetsDuringQ2$tweetuser %in% residentList,]

##GeoCode to County map
for(i in 1:nrow(residentTweetsQ2)) {
  Lat <- as.double(residentTweetsQ2[i,"lat"]) 
  Lon <- as.double(residentTweetsQ2[i,"long"])
  #make a data frame
  coords <- as.data.frame(cbind(Lon,Lat))
  #and into Spatial
  points <- SpatialPoints(coords)
  #assume same proj as shapefile!
  proj4string(points) <- proj4string(us.map)
  #get county polygon point is in
  residentTweetsQ2[i, "countyFIPS"] <- as.character(over(points, us.map)$GEOID)
}

#### Save residentTweetsQ2 for fututre use ####

saveRDS(residentTweetsQ2, "residentTweetsQ2.Rds")
#residentTweetsQ2 <- readRDS("residentTweetsQ2.Rds")

##Delete null counties
residentTweetsQ2 <- residentTweetsQ2[!(is.na(residentTweetsQ2$countyFIPS) | residentTweetsQ2$countyFIPS==""), ]

##No. of tweets per county
tweetCountPerCountyQ2 <- residentTweetsQ2 %>%
  group_by(tweetuser,countyFIPS) %>%
  summarize(n())
tweetCountPerCountyQ2 <- aggregate(tweetCountPerCountyQ2$`n()`, by=list(GEOID=tweetCountPerCountyQ2$countyFIPS), FUN=sum)

### Visualization ###

#Rename countyfips column name to match shapefile
colnames(tweetCountPerCountyQ2) <- c("GEOID", "count")

tweetCountPerCountyQ2$GEOID <- formatC(tweetCountPerCountyQ2$GEOID, width = 5, format = "d", flag = "0")

#Merge shapefile with data file
leafmap <- merge(us.map, tweetCountPerCountyQ2, by=c("GEOID"))
leafmap[is.na(leafmap$count)] <- 0

leafmap <- leafmap[leafmap$count > 0,]

#Define popup info
popup_dat <- paste0("<strong>County: </strong>", 
                    leafmap$NAME, 
                    "<br><strong>Tweet User Count: </strong>", 
                    leafmap$count)

#Define color pallete
pal <- colorNumeric("RdYlBu", domain=0:max(leafmap$count))

#Print resident tweet count per county in the selected counties
leaflet(data = leafmap) %>% addTiles() %>%
  addPolygons(fillColor = ~pal(count), 
              stroke = TRUE,
              color = "blue",
              opacity = 5,
              fillOpacity = 5, 
              weight = 0.2, 
              smoothFactor = 0.2,  
              popup = popup_dat) %>%
  addLegend(pal = pal, values = 0:max(leafmap$count), position = "topright")
