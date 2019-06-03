library(RPostgreSQL)
library(sqldf)
library(ggplot2)
library(syuzhet)

residentList <- readRDS("/home/cynthiak/Irma/residents.Rds")

drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='cynthiak',password='Kamalendra1985')

#Getting all the tweets with geocoded information
allTweetsDay8<-dbGetQuery(conn,
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
 tweettext
FROM tweets_info 
WHERE tweetcreated BETWEEN '2017-09-11' AND '2017-09-12';
")

residentTweetsDay8<- allTweetsDay8[allTweetsDay8$tweetuser %in% residentList,]

tweet_day8<- residentTweetsDay8$tweettext
#convert all text to lower case
tweet_day8<- tolower(tweet_day8)
# Replace blank space (“rt”)
tweet_day8 <- gsub("rt", "", tweet_day8)
# Replace @UserName
tweet_day8 <- gsub("@\\w+", "", tweet_day8)
# Remove punctuation
tweet_day8 <- gsub("[[:punct:]]", "", tweet_day8)
# Remove links
tweet_day8 <- gsub("http\\w+", "", tweet_day8)
# Remove tabs
tweet_day8 <- gsub("[ |\t]{2,}", "", tweet_day8)
# Remove blank spaces at the beginning
tweet_day8 <- gsub("^ ", "", tweet_day8)
# Remove blank spaces at the end
tweet_day8 <- gsub(" $", "", tweet_day8)

#getting emotions using in-built function
sentiment_day8<-get_nrc_sentiment((tweet_day8))
#calculationg total score for each sentiment
Sentimentscores_day8<-data.frame(colSums(sentiment_day8[,]))

names(Sentimentscores_day8)<-"Score"
Sentimentscores_day8<-cbind("day"=rep(c(8),10), "sentiment"=rownames(Sentimentscores_day8),Sentimentscores_day8)
rownames(Sentimentscores_day8)<-NULL

saveRDS(Sentimentscores_day8, "day8.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day8,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 11th, 2017")
