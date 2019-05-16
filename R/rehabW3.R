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
allTweetsAfterW3<-dbGetQuery(conn,
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
AND tweetcreated BETWEEN '2017-09-01' AND '2017-09-07'
GROUP BY  tweetuser,date(tweetcreated),lat,long,tweetpname
ORDER BY date(tweetcreated);
")

residentTweetsW3<- allTweetsAfterW3[allTweetsAfterW3$tweetuser %in% residentList,]

##GeoCode to County map
for(i in 1:nrow(residentTweetsW3)) {
  Lat13 <- as.double(residentTweetsW3[i,"lat"]) 
  Lon13 <- as.double(residentTweetsW3[i,"long"])
  #make a data frame
  coords13 <- as.data.frame(cbind(Lon13,Lat13))
  #and into Spatial
  points13 <- SpatialPoints(coords13)
  #assume same proj as shapefile!
  proj4string(points13) <- proj4string(us.map)
  #get county polygon point is in
  residentTweetsW3[i, "GEOID"] <- as.character(over(points13, us.map)$GEOID)
}

#### Save ResidentTweetsW3 for fututre use ####

saveRDS(residentTweetsW3, "residentTweetsW3.Rds")
#residentTweetsW3 <- readRDS("residentTweetsW3.Rds")

##Delete null counties
residentTweetsW3 <- residentTweetsW3[!(is.na(residentTweetsW3$GEOID) | residentTweetsW3$GEOID==""), ]

##No. of tweets per county
tweetCountPerCountyW3 <- residentTweetsW3 %>%
  group_by(tweetuser,GEOID) %>%
  summarize(n())
tweetCountPerCountyW3 <- aggregate(tweetCountPerCountyW3$`n()`, by=list(GEOID=tweetCountPerCountyW3$GEOID), FUN=sum)

### Visualization ###

#Rename countyfips column name to match shapefile
colnames(tweetCountPerCountyW3) <- c("GEOID", "count")

tweetCountPerCountyW3$GEOID <- formatC(tweetCountPerCountyW3$GEOID, width = 5, format = "d", flag = "0")

#Merge shapefile with data file
leafmap <- merge(us.map, tweetCountPerCountyW3, by=c("GEOID"))
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
              popup = popup_dat) %>%
  addLegend(pal = pal, values = 5:max(leafmap$count), position = "topright")
