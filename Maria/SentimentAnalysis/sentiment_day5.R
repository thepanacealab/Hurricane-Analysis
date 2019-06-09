
#Getting all the tweets with geocoded information
allTweetsDay5<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-09-20' AND '2017-09-21';
")

residentTweetsDay5<- allTweetsDay5[allTweetsDay5$tweetuser %in% residentList,]

tweet_day5<- residentTweetsDay5$tweettext
#convert all text to lower case
tweet_day5<- tolower(tweet_day5)
# Replace blank space (“rt”)
tweet_day5 <- gsub("rt", "", tweet_day5)
# Replace @UserName
tweet_day5 <- gsub("@\\w+", "", tweet_day5)
# Remove punctuation
tweet_day5 <- gsub("[[:punct:]]", "", tweet_day5)
# Remove links
tweet_day5 <- gsub("http\\w+", "", tweet_day5)
# Remove tabs
tweet_day5 <- gsub("[ |\t]{2,}", "", tweet_day5)
# Remove blank spaces at the beginning
tweet_day5 <- gsub("^ ", "", tweet_day5)
# Remove blank spaces at the end
tweet_day5 <- gsub(" $", "", tweet_day5)

#getting emotions using in-built function
sentiment_day5<-get_nrc_sentiment((tweet_day5))
#calculationg total score for each sentiment
Sentimentscores_day5<-data.frame(colSums(sentiment_day5[,]))

names(Sentimentscores_day5)<-"Score"
Sentimentscores_day5<-cbind("day"=rep(c(20),10), "sentiment"=rownames(Sentimentscores_day5),Sentimentscores_day5)
rownames(Sentimentscores_day5)<-NULL

saveRDS(Sentimentscores_day5, "day5.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day5,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 20th, 2017")
