library(RPostgreSQL)
library(sqldf)
library(ggplot2)
library(syuzhet)

residentList <- readRDS("/home/cynthiak/Irma/residents.Rds")

drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='####',password='####')

#Getting all the tweets with geocoded information
allTweetsDay10<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-09-13' AND '2017-09-14';
")

residentTweetsDay10<- allTweetsDay10[allTweetsDay10$tweetuser %in% residentList,]

tweet_day10<- residentTweetsDay10$tweettext
#convert all text to lower case
tweet_day10<- tolower(tweet_day10)
# Replace blank space (“rt”)
tweet_day10 <- gsub("rt", "", tweet_day10)
# Replace @UserName
tweet_day10 <- gsub("@\\w+", "", tweet_day10)
# Remove punctuation
tweet_day10 <- gsub("[[:punct:]]", "", tweet_day10)
# Remove links
tweet_day10 <- gsub("http\\w+", "", tweet_day10)
# Remove tabs
tweet_day10 <- gsub("[ |\t]{2,}", "", tweet_day10)
# Remove blank spaces at the beginning
tweet_day10 <- gsub("^ ", "", tweet_day10)
# Remove blank spaces at the end
tweet_day10 <- gsub(" $", "", tweet_day10)

#getting emotions using in-built function
sentiment_day10<-get_nrc_sentiment((tweet_day10))
#calculationg total score for each sentiment
Sentimentscores_day10<-data.frame(colSums(sentiment_day10[,]))

names(Sentimentscores_day10)<-"Score"
Sentimentscores_day10<-cbind("day"=rep(c(10),10), "sentiment"=rownames(Sentimentscores_day10),Sentimentscores_day10)
rownames(Sentimentscores_day10)<-NULL

saveRDS(Sentimentscores_day10, "day10.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day10,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 13th, 2017")
