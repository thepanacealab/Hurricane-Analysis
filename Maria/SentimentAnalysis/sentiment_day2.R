library(RPostgreSQL)
library(sqldf)
library(ggplot2)
library(syuzhet)

residentList <- readRDS("/home/cynthiak/Maria/residentsMaria.Rds")

drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='cynthiak',password='Kamalendra1985')

#Getting all the tweets with geocoded information
allTweetsDay2<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-09-17' AND '2017-09-18';
")

residentTweetsDay2<- allTweetsDay2[allTweetsDay2$tweetuser %in% residentList,]

tweet_day2<- residentTweetsDay2$tweettext
#convert all text to lower case
tweet_day2<- tolower(tweet_day2)
# Replace blank space (“rt”)
tweet_day2 <- gsub("rt", "", tweet_day2)
# Replace @UserName
tweet_day2 <- gsub("@\\w+", "", tweet_day2)
# Remove punctuation
tweet_day2 <- gsub("[[:punct:]]", "", tweet_day2)
# Remove links
tweet_day2 <- gsub("http\\w+", "", tweet_day2)
# Remove tabs
tweet_day2 <- gsub("[ |\t]{2,}", "", tweet_day2)
# Remove blank spaces at the beginning
tweet_day2 <- gsub("^ ", "", tweet_day2)
# Remove blank spaces at the end
tweet_day2 <- gsub(" $", "", tweet_day2)

#getting emotions using in-built function
sentiment_day2<-get_nrc_sentiment((tweet_day2))
#calculationg total score for each sentiment
Sentimentscores_day2<-data.frame(colSums(sentiment_day2[,]))

names(Sentimentscores_day2)<-"Score"
Sentimentscores_day2<-cbind("day"=rep(c(17),10), "sentiment"=rownames(Sentimentscores_day2),Sentimentscores_day2)
rownames(Sentimentscores_day2)<-NULL

saveRDS(Sentimentscores_day2, "day2.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day2,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 17th, 2017")

