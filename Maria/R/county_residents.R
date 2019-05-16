library(RPostgreSQL)
library(sqldf)
library(dplyr)
library(sp)
library(rgdal)

########## Hurricane Maria Counties ############

tweetcountyMaria<-data.frame("FIPS" = c("72001","72003","72005","72007","72009","72011",
                                        "72013","72015","72017","72019","72021","72023",
                                        "72025","72027","72029","72031","72033","72035",
                                        "72037","72041","72043","72045","72047","72049",
                                        "72051","72053","72054","72055","72057","72059",
                                        "72061","72063","72065","72067","72071","72073",
                                        "72075","72077","72079","72081","72083","72085",
                                        "72087","72089","72091","72093","72095","72097",
                                        "72099","72101","72103","72105","72107","72109",
                                        "72111","72113","72115","72117","72119","72121",
                                        "72123","72125","72127","72129","72131","72133",
                                        "72135","72137","72139","72141","72143","72145",
                                        "72147","72149","72151","72153","78010","78020",
                                        "78030","37055","37095","37137","37031","37177",
                                        "37053"))
drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='####',password='####')

counties <- readOGR("/home/cynthiak/county", "tl_2018_us_county", stringsAsFactors = FALSE)
### Subset the counties to only the ones of interest
testMaria<- counties[counties$GEOID %in% tweetcountyMaria$FIPS,]


#Getting all the tweets with geocoded information
allGeoCodedTweetsMaria<-dbGetQuery(conn,
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
AND tweetcreated BETWEEN '2017-08-01' AND '2017-08-31'
GROUP BY  tweetuser,date(tweetcreated),lat,long,tweetpname
HAVING COUNT(tweettext) > 1
ORDER BY date(tweetcreated);
")


for(i in 1:nrow(allGeoCodedTweetsMaria)) {
  Lat <- as.double(allGeoCodedTweetsMaria[i,"lat"]) 
  Lon <- as.double(allGeoCodedTweetsMaria[i,"long"])
  #make a data frame
  coords <- as.data.frame(cbind(Lon,Lat))
  #and into Spatial
  points <- SpatialPoints(coords)
  #assume same proj as shapefile!
  proj4string(points) <- proj4string(testMaria)
  #get county polygon point is in
  allGeoCodedTweetsMaria[i, "countyFIPS"] <- as.character(over(points, testMaria)$GEOID)
}

#### Save allGeoCodedTweetsMaria ####

saveRDS(allGeoCodedTweetsMaria, "allGeoCodedTweetsMaria.Rds")
#allGeoCodedTweetsMaria <- readRDS("allGeoCodedTweetsMaria.Rds")

##Delete records outside the hurricane counties
validGeoCodedTweetsMaria <- allGeoCodedTweetsMaria[!(is.na(allGeoCodedTweetsMaria$countyFIPS) | allGeoCodedTweetsMaria$countyFIPS==""), ]

##Find distinct users list
residentsMaria <- unique(validGeoCodedTweetsMaria$tweetuser)

######### Visualization #############

##No. of tweets per county
tweetCountPerCountyM <- count(validGeoCodedTweetsMaria, countyFIPS)

#Rename countyfips column name to match shapefile
colnames(tweetCountPerCountyM) <- c("GEOID", "count")

tweetCountPerCountyM$GEOID <- formatC(tweetCountPerCountyM$GEOID, width = 5, format = "d", flag = "0")

#Merge shapefile with data file
leafmap <- merge(testMaria, tweetCountPerCountyM, by=c("GEOID"))
leafmap[is.na(leafmap$count)] <- 0

#Define popup info
popup_dat <- paste0("<strong>County: </strong>", 
                    leafmap$NAME, 
                    "<br><strong>Evacuation Status: </strong>", 
                    leafmap$count)

#Define color pallete
pal <- colorNumeric("RdYlBu", domain=min(leafmap$count):max(leafmap$count))

#Print resident tweet count per county in the selected counties
leaflet(data = leafmap) %>% addTiles() %>%
  addPolygons(fillColor = ~pal(count), 
              stroke = TRUE,
              color = "blue",
              opacity = 1,
              fillOpacity = 5, 
              weight = 0.2, 
              smoothFactor = 0.2,  
              popup = popup_dat) %>%
  addLegend(pal = pal, values = min(leafmap$count):max(leafmap$count), position = "topright")

#### Save Resident vector for fututre use ####

saveRDS(residentsMaria, "residentsMaria.Rds")
