library(RPostgreSQL)
library(sqldf)
library(ggplot2)
library(syuzhet)

residentList <- readRDS("/home/cynthiak/Irma/residents.Rds")

drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='cynthiak',password='Kamalendra1985')

#Getting all the tweets with geocoded information
allTweetsDay7<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-09-10' AND '2017-09-11';
")

residentTweetsDay7<- allTweetsDay7[allTweetsDay7$tweetuser %in% residentList,]

tweet_day7<- residentTweetsDay7$tweettext
#convert all text to lower case
tweet_day7<- tolower(tweet_day7)
# Replace blank space (“rt”)
tweet_day7 <- gsub("rt", "", tweet_day7)
# Replace @UserName
tweet_day7 <- gsub("@\\w+", "", tweet_day7)
# Remove punctuation
tweet_day7 <- gsub("[[:punct:]]", "", tweet_day7)
# Remove links
tweet_day7 <- gsub("http\\w+", "", tweet_day7)
# Remove tabs
tweet_day7 <- gsub("[ |\t]{2,}", "", tweet_day7)
# Remove blank spaces at the beginning
tweet_day7 <- gsub("^ ", "", tweet_day7)
# Remove blank spaces at the end
tweet_day7 <- gsub(" $", "", tweet_day7)

#getting emotions using in-built function
sentiment_day7<-get_nrc_sentiment((tweet_day7))
#calculationg total score for each sentiment
Sentimentscores_day7<-data.frame(colSums(sentiment_day7[,]))

names(Sentimentscores_day7)<-"Score"
Sentimentscores_day7<-cbind("day"=rep(c(7),10), "sentiment"=rownames(Sentimentscores_day7),Sentimentscores_day7)
rownames(Sentimentscores_day7)<-NULL

saveRDS(Sentimentscores_day7, "day7.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day7,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 10th, 2017")
