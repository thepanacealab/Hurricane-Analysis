library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(dplyr)
library(leaflet)
library(scales)

#Prep data file
#County FIPS downloaded from https://www2.census.gov/programs-surveys/popest/geographies/2017/all-geocodes-v2017.xlsx
dat <- read.csv(file="irma_evacuation_status.csv", header=TRUE, sep=",")

#Change column names to lower case
names(dat) <- tolower(names(dat))

#Select subset of necessary columns 
county_dat <- subset(dat, select = c("statecode.fips.", "countyfips", "areaname","status"))

#Rename countyfips column name to match shapefile
colnames(county_dat) <- c("statecode","GEOID", "areaname","status")

county_dat$GEOID <- formatC(county_dat$GEOID, width = 5, format = "d", flag = "0")

#Read shapefile 
#Shapefile downloaded from https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2018&layergroup=Counties+%28and+equivalent%29
us.map <- readOGR("/home/cynthiak/county", "tl_2018_us_county", stringsAsFactors = FALSE)

#Select Florida(12) and Georgia(13) subset
us.map <- us.map[us.map$STATEFP %in% c("12", "13"),]

#Merge shapefile with data file
leafmap <- merge(us.map, county_dat, by=c("GEOID"))

#Define popup info
popup_dat <- paste0("<strong>County: </strong>", 
                     leafmap$NAME, 
                     "<br><strong>Evacuation Status: </strong>", 
                     leafmap$status)

#Define pallete: Yellow = Emergency, Orange = Voluntary, Red = Mandatory
pal <- colorFactor(c('yellow', 'red', 'orange'),
                            domain = leafmap$status)

#Print evacuation status per county in Florida and Georgia
leaflet(data = leafmap) %>% addTiles() %>%
     addPolygons(fillColor = ~pal(status), 
                 fillOpacity = 0.8, 
                 color = "#BDBDC3", 
                 weight = 1,
                 popup = popup_dat)


