library(RPostgreSQL)
library(sqldf)
library(ggplot2)
library(syuzhet)

residentList <- readRDS("/home/cynthiak/Maria/residentsMaria.Rds")

drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='cynthiak',password='Kamalendra1985')

#Getting all the tweets with geocoded information
allTweetsDay1<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-09-16' AND '2017-09-17';
")

residentTweetsDay1<- allTweetsDay1[allTweetsDay1$tweetuser %in% residentList,]

tweet_day1<- residentTweetsDay1$tweettext
#convert all text to lower case
tweet_day1<- tolower(tweet_day1)
# Replace blank space (“rt”)
tweet_day1 <- gsub("rt", "", tweet_day1)
# Replace @UserName
tweet_day1 <- gsub("@\\w+", "", tweet_day1)
# Remove punctuation
tweet_day1 <- gsub("[[:punct:]]", "", tweet_day1)
# Remove links
tweet_day1 <- gsub("http\\w+", "", tweet_day1)
# Remove tabs
tweet_day1 <- gsub("[ |\t]{2,}", "", tweet_day1)
# Remove blank spaces at the beginning
tweet_day1 <- gsub("^ ", "", tweet_day1)
# Remove blank spaces at the end
tweet_day1 <- gsub(" $", "", tweet_day1)

#getting emotions using in-built function
sentiment_day1<-get_nrc_sentiment((tweet_day1))
#calculationg total score for each sentiment
Sentimentscores_day1<-data.frame(colSums(sentiment_day1[,]))

names(Sentimentscores_day1)<-"Score"
Sentimentscores_day1<-cbind("day"=rep(c(16),10), "sentiment"=rownames(Sentimentscores_day1),Sentimentscores_day1)
rownames(Sentimentscores_day1)<-NULL

saveRDS(Sentimentscores_day1, "day1.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day1,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 16th, 2017")

