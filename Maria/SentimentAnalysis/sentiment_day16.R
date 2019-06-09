library(RPostgreSQL)
library(sqldf)
library(ggplot2)
library(syuzhet)

residentList <- readRDS("/home/cynthiak/Maria/residentsMaria.Rds")

drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='cynthiak',password='Kamalendra1985')

#Getting all the tweets with geocoded information
allTweetsDay16<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-10-01' AND '2017-10-02';
")

residentTweetsDay16<- allTweetsDay16[allTweetsDay16$tweetuser %in% residentList,]

tweet_day16<- residentTweetsDay16$tweettext
#convert all text to lower case
tweet_day16<- tolower(tweet_day16)
# Replace blank space (“rt”)
tweet_day16 <- gsub("rt", "", tweet_day16)
# Replace @UserName
tweet_day16 <- gsub("@\\w+", "", tweet_day16)
# Remove punctuation
tweet_day16 <- gsub("[[:punct:]]", "", tweet_day16)
# Remove links
tweet_day16 <- gsub("http\\w+", "", tweet_day16)
# Remove tabs
tweet_day16 <- gsub("[ |\t]{2,}", "", tweet_day16)
# Remove blank spaces at the beginning
tweet_day16 <- gsub("^ ", "", tweet_day16)
# Remove blank spaces at the end
tweet_day16 <- gsub(" $", "", tweet_day16)

#getting emotions using in-built function
sentiment_day16<-get_nrc_sentiment((tweet_day16))
#calculationg total score for each sentiment
Sentimentscores_day16<-data.frame(colSums(sentiment_day16[,]))

names(Sentimentscores_day16)<-"Score"
Sentimentscores_day16<-cbind("day"=rep(c(31),10), "sentiment"=rownames(Sentimentscores_day16),Sentimentscores_day16)
rownames(Sentimentscores_day16)<-NULL

saveRDS(Sentimentscores_day16, "day16.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day16,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Oct 1st, 2017")

