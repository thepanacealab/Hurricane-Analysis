library(RPostgreSQL)
library(sqldf)
library(ggplot2)
library(syuzhet)

residentList <- readRDS("/home/cynthiak/Irma/residents.Rds")

drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='cynthiak',password='Kamalendra1985')

#Getting all the tweets with geocoded information
allTweetsDay6<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-09-09' AND '2017-09-10';
")

residentTweetsDay6<- allTweetsDay6[allTweetsDay6$tweetuser %in% residentList,]

tweet_day6<- residentTweetsDay6$tweettext
#convert all text to lower case
tweet_day6<- tolower(tweet_day6)
# Replace blank space (“rt”)
tweet_day6 <- gsub("rt", "", tweet_day6)
# Replace @UserName
tweet_day6 <- gsub("@\\w+", "", tweet_day6)
# Remove punctuation
tweet_day6 <- gsub("[[:punct:]]", "", tweet_day6)
# Remove links
tweet_day6 <- gsub("http\\w+", "", tweet_day6)
# Remove tabs
tweet_day6 <- gsub("[ |\t]{2,}", "", tweet_day6)
# Remove blank spaces at the beginning
tweet_day6 <- gsub("^ ", "", tweet_day6)
# Remove blank spaces at the end
tweet_day6 <- gsub(" $", "", tweet_day6)

#getting emotions using in-built function
sentiment_day6<-get_nrc_sentiment((tweet_day6))
#calculationg total score for each sentiment
Sentimentscores_day6<-data.frame(colSums(sentiment_day6[,]))

names(Sentimentscores_day6)<-"Score"
Sentimentscores_day6<-cbind("day"=rep(c(6),10), "sentiment"=rownames(Sentimentscores_day6),Sentimentscores_day6)
rownames(Sentimentscores_day6)<-NULL

saveRDS(Sentimentscores_day6, "day6.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day6,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 9th, 2017")
