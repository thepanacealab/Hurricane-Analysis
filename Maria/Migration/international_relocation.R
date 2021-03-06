library(dplyr)
library(maps)
library(geosphere)

data <- read.csv(file="/home/cynthiak/Maria/Relocation/mr4_tweets.csv", header=TRUE, sep=",")


pr <- read.csv(file="/home/cynthiak/Maria/Relocation/pr.csv", header=TRUE, sep=",")

#create basemap
#map("world", regions=c("usa"), fill=T, col="grey8", bg="grey15", ylim=c(21.0,55.0), xlim=c(-130.0,-65.0))
map("world", fill=T, col="grey8", bg="grey15", ylim=c(-70,100), mar=c(0,0,0,0))

title(main="Tweet locations worldwide for PuertoRico/NC Residents from 10/24/2017 to 10/30/2017",
      cex.main = 1,   font.main= 1, col.main= "white")
#overlay user locations
points(data$long,data$lat, pch=3, cex=0.1, col="chocolate1")

for (i in (1:dim(data)[1])) { 
  inter <- gcIntermediate(c(pr$long[1], pr$lat[1]), c(data$long[i], data$lat[i]), n=200)
  lines(inter, lwd=0.1, col="turquoise2")    
}

legend("bottomleft", 
       legend=c("Resident Tweet Location", "Distance Travelled"), text.col = "gray60",
       bty = "n",lwd=0.75, cex=0.75, y.intersp=0.75, col=c("chocolate1", "turquoise2"), lty=c(NA, 1), pch = c(8, NA))

+