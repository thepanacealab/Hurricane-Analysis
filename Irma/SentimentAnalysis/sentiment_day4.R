library(RPostgreSQL)
library(sqldf)
library(ggplot2)
library(syuzhet)

residentList <- readRDS("/home/cynthiak/Irma/residents.Rds")

drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='####',password='####')

#Getting all the tweets with geocoded information
allTweetsDay4<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-09-07' AND '2017-09-08';
")

residentTweetsDay4<- allTweetsDay4[allTweetsDay4$tweetuser %in% residentList,]

tweet_day4<- residentTweetsDay4$tweettext
#convert all text to lower case
tweet_day4<- tolower(tweet_day4)
# Replace blank space (“rt”)
tweet_day4 <- gsub("rt", "", tweet_day4)
# Replace @UserName
tweet_day4 <- gsub("@\\w+", "", tweet_day4)
# Remove punctuation
tweet_day4 <- gsub("[[:punct:]]", "", tweet_day4)
# Remove links
tweet_day4 <- gsub("http\\w+", "", tweet_day4)
# Remove tabs
tweet_day4 <- gsub("[ |\t]{2,}", "", tweet_day4)
# Remove blank spaces at the beginning
tweet_day4 <- gsub("^ ", "", tweet_day4)
# Remove blank spaces at the end
tweet_day4 <- gsub(" $", "", tweet_day4)

#getting emotions using in-built function
sentiment_day4<-get_nrc_sentiment((tweet_day4))
#calculationg total score for each sentiment
Sentimentscores_day4<-data.frame(colSums(sentiment_day4[,]))

names(Sentimentscores_day4)<-"Score"
Sentimentscores_day4<-cbind("day"=rep(c(4),10), "sentiment"=rownames(Sentimentscores_day4),Sentimentscores_day4)
rownames(Sentimentscores_day4)<-NULL

saveRDS(Sentimentscores_day4, "day4.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day4,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 7th, 2017")
