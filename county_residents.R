library(RPostgreSQL)
library(sqldf)
library(dplyr)
library(sp)
library(rgdal)

########### Hurricane Counties #####################
tweetcountyIrma<-data.frame("FIPS" = c("12131","12005","12045","12047","12125","13237","12069","12097","12055","12093","12051","12063","12065","12129","12037","12123","12121","12067","12029","12041","12003",
                                              "12075","12083","12017","12119","12053","12101","12057","12103","12019","12031","12089","12109","12035",
                                              "12127","12117","12095","12105","12081","12049","12115","12027","12015","12043","12071","12021","12087",
                                              "12086","12011","12099","12085","12009","12111","12061","13001","13003","13005","13025","13029","13031","13033","13039","13043",
                                              "13049","13051","13065","13069","13101","13103","13109","13127","13165",
                                              "13161","13179","13183","13191","13229","13251","13267","13279","13283","13305","13299"))

tweetcountyMaria<-data.frame("FIPS" = c("72001","72003","72005","72007","72009","72011","72013","72015","72017","72019","72021","72023","72025","72027","72029","72031","72033","72035","72037","72041","72043","72045","72047","72049","72051","72053","72054","72055","72057","72059","72061","72063","72065","72067","72071","72073","72075","72077","72079","72081","72083","72085","72087","72089","72091","72093","72095","72097","72099","72101","72103","72105","72107","72109","72111","72113","72115","72117","72119","72121","72123","72125","72127","72129","72131","72133","72135","72137","72139","72141","72143","72145","72147","72149","72151","72153"))

tweetcounties_all<-rbind(tweetcountyIrma, tweetcountyMaria)

drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='#####',password='#####')

counties <- readOGR("/home/cynthiak/county", "tl_2018_us_county", stringsAsFactors = FALSE)
### Subset the counties to only the ones of interest
test<- counties[counties$GEOID %in% tweetcounties_all$FIPS,]

#Getting all the tweets with geocoded information
allGeoCodedTweets<-dbGetQuery(conn,
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
AND tweetcreated BETWEEN '2017-07-31' AND '2017-08-30'
GROUP BY  tweetuser,date(tweetcreated),lat,long,tweetpname
HAVING COUNT(tweettext) > 1
ORDER BY date(tweetcreated);
")


for(i in 1:nrow(allGeoCodedTweets)) {
  Lat <- as.double(allGeoCodedTweets[i,"lat"]) 
  Lon <- as.double(allGeoCodedTweets[i,"long"])
  #make a data frame
  coords <- as.data.frame(cbind(Lon,Lat))
  #and into Spatial
  points <- SpatialPoints(coords)
  #assume same proj as shapefile!
  proj4string(points) <- proj4string(test)
  #get county polygon point is in
  allGeoCodedTweets[i, "countyFIPS"] <- as.character(over(points, test)$GEOID)
}

#### Save allGeoCodedTweets ####

saveRDS(allGeoCodedTweets, "allGeoCodedTweets.Rds")
#allGeoCodedTweets <- readRDS("allGeoCodedTweets.Rds")

##Delete records outside the hurricane counties
validGeoCodedTweets <- allGeoCodedTweets[!(is.na(allGeoCodedTweets$countyFIPS) | allGeoCodedTweets$countyFIPS==""), ]

##Find distinct users list
residents <- unique(validGeoCodedTweets$tweetuser)

######### Visualization #############

##No. of tweets per county
tweetCountPerCounty <- count(validGeoCodedTweets, countyFIPS)

#Rename countyfips column name to match shapefile
colnames(tweetCountPerCounty) <- c("GEOID", "count")

tweetCountPerCounty$GEOID <- formatC(tweetCountPerCounty$GEOID, width = 5, format = "d", flag = "0")

#Merge shapefile with data file
leafmap <- merge(test, tweetCountPerCounty, by=c("GEOID"))
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
              opacity = 5,
              fillOpacity = 5, 
              weight = 0.2, 
              smoothFactor = 0.2,  
              popup = popup_dat)

#### Save Resident vector for fututre use ####

saveRDS(residents, "residents.Rds")
