library(RPostgreSQL)
library(sqldf)
library(ggplot2)
library(syuzhet)

residentList <- readRDS("/home/cynthiak/Maria/residentsMaria.Rds")

drv <- dbDriver("PostgreSQL")
conn <-dbConnect(drv,host='localhost',port='5432',dbname='psh_tweets',user='cynthiak',password='Kamalendra1985')

#Getting all the tweets with geocoded information
allTweetsDay15<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-09-30' AND '2017-10-01';
")

residentTweetsDay15<- allTweetsDay15[allTweetsDay15$tweetuser %in% residentList,]

tweet_day15<- residentTweetsDay15$tweettext
#convert all text to lower case
tweet_day15<- tolower(tweet_day15)
# Replace blank space (“rt”)
tweet_day15 <- gsub("rt", "", tweet_day15)
# Replace @UserName
tweet_day15 <- gsub("@\\w+", "", tweet_day15)
# Remove punctuation
tweet_day15 <- gsub("[[:punct:]]", "", tweet_day15)
# Remove links
tweet_day15 <- gsub("http\\w+", "", tweet_day15)
# Remove tabs
tweet_day15 <- gsub("[ |\t]{2,}", "", tweet_day15)
# Remove blank spaces at the beginning
tweet_day15 <- gsub("^ ", "", tweet_day15)
# Remove blank spaces at the end
tweet_day15 <- gsub(" $", "", tweet_day15)

#getting emotions using in-built function
sentiment_day15<-get_nrc_sentiment((tweet_day15))
#calculationg total score for each sentiment
Sentimentscores_day15<-data.frame(colSums(sentiment_day15[,]))

names(Sentimentscores_day15)<-"Score"
Sentimentscores_day15<-cbind("day"=rep(c(30),10), "sentiment"=rownames(Sentimentscores_day15),Sentimentscores_day15)
rownames(Sentimentscores_day15)<-NULL

saveRDS(Sentimentscores_day15, "day15.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day15,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 30th, 2017")

