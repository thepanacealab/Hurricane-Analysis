library(RPostgreSQL)
library(sqldf)
library(ggplot2)
library(syuzhet)

residentList <- readRDS("/home/cynthiak/Maria/residentsMaria.Rds")

drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='cynthiak',password='Kamalendra1985')

#Getting all the tweets with geocoded information
allTweetsDay13<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-09-28' AND '2017-09-29';
")

residentTweetsDay13<- allTweetsDay13[allTweetsDay13$tweetuser %in% residentList,]

tweet_day13<- residentTweetsDay13$tweettext
#convert all text to lower case
tweet_day13<- tolower(tweet_day13)
# Replace blank space (“rt”)
tweet_day13 <- gsub("rt", "", tweet_day13)
# Replace @UserName
tweet_day13 <- gsub("@\\w+", "", tweet_day13)
# Remove punctuation
tweet_day13 <- gsub("[[:punct:]]", "", tweet_day13)
# Remove links
tweet_day13 <- gsub("http\\w+", "", tweet_day13)
# Remove tabs
tweet_day13 <- gsub("[ |\t]{2,}", "", tweet_day13)
# Remove blank spaces at the beginning
tweet_day13 <- gsub("^ ", "", tweet_day13)
# Remove blank spaces at the end
tweet_day13 <- gsub(" $", "", tweet_day13)

#getting emotions using in-built function
sentiment_day13<-get_nrc_sentiment((tweet_day13))
#calculationg total score for each sentiment
Sentimentscores_day13<-data.frame(colSums(sentiment_day13[,]))

names(Sentimentscores_day13)<-"Score"
Sentimentscores_day13<-cbind("day"=rep(c(28),10), "sentiment"=rownames(Sentimentscores_day13),Sentimentscores_day13)
rownames(Sentimentscores_day13)<-NULL

saveRDS(Sentimentscores_day13, "day13.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day13,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 28th, 2017")

