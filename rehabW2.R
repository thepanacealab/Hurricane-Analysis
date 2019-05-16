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
allTweetsAfterW2<-dbGetQuery(conn,
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
AND tweetcreated BETWEEN '2017-08-24' AND '2017-08-30'
GROUP BY  tweetuser,date(tweetcreated),lat,long,tweetpname
ORDER BY date(tweetcreated);
")

residentTweetsW2<- allTweetsAfterW2[allTweetsAfterW2$tweetuser %in% residentList,]

##GeoCode to County map
for(i in 1:nrow(residentTweetsW2)) {
  Lat12 <- as.double(residentTweetsW2[i,"lat"]) 
  Lon12 <- as.double(residentTweetsW2[i,"long"])
  #make a data frame
  coords12 <- as.data.frame(cbind(Lon12,Lat12))
  #and into Spatial
  points12 <- SpatialPoints(coords12)
  #assume same proj as shapefile!
  proj4string(points12) <- proj4string(us.map)
  #get county polygon point is in
  residentTweetsW2[i, "GEOID"] <- as.character(over(points12, us.map)$GEOID)
}

#### Save ResidentTweetsW2 for fututre use ####

saveRDS(residentTweetsW2, "residentTweetsW2.Rds")
#residentTweetsW2 <- readRDS("residentTweetsW2.Rds")

##Delete null counties
residentTweetsW2 <- residentTweetsW2[!(is.na(residentTweetsW2$GEOID) | residentTweetsW2$GEOID==""), ]

##No. of tweets per county
tweetCountPerCountyW2 <- residentTweetsW2 %>%
  group_by(tweetuser,GEOID) %>%
  summarize(n())
tweetCountPerCountyW2
tweetCountPerCountyW2 <- aggregate(tweetCountPerCountyW2$`n()`, by=list(GEOID=tweetCountPerCountyW2$GEOID), FUN=sum)

### Visualization ###

#Rename countyfips column name to match shapefile
colnames(tweetCountPerCountyW2) <- c("GEOID", "count")

tweetCountPerCountyW2$GEOID <- formatC(tweetCountPerCountyW2$GEOID, width = 5, format = "d", flag = "0")

#Merge shapefile with data file
leafmap <- merge(us.map, tweetCountPerCountyW2, by=c("GEOID"))
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
