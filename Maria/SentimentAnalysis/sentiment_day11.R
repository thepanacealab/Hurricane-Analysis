#Getting all the tweets with geocoded information
allTweetsDay11<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-09-26' AND '2017-09-27';
")

residentTweetsDay11<- allTweetsDay11[allTweetsDay11$tweetuser %in% residentList,]

tweet_day11<- residentTweetsDay11$tweettext
#convert all text to lower case
tweet_day11<- tolower(tweet_day11)
# Replace blank space (“rt”)
tweet_day11 <- gsub("rt", "", tweet_day11)
# Replace @UserName
tweet_day11 <- gsub("@\\w+", "", tweet_day11)
# Remove punctuation
tweet_day11 <- gsub("[[:punct:]]", "", tweet_day11)
# Remove links
tweet_day11 <- gsub("http\\w+", "", tweet_day11)
# Remove tabs
tweet_day11 <- gsub("[ |\t]{2,}", "", tweet_day11)
# Remove blank spaces at the beginning
tweet_day11 <- gsub("^ ", "", tweet_day11)
# Remove blank spaces at the end
tweet_day11 <- gsub(" $", "", tweet_day11)

#getting emotions using in-built function
sentiment_day11<-get_nrc_sentiment((tweet_day11))
#calculationg total score for each sentiment
Sentimentscores_day11<-data.frame(colSums(sentiment_day11[,]))

names(Sentimentscores_day11)<-"Score"
Sentimentscores_day11<-cbind("day"=rep(c(26),10), "sentiment"=rownames(Sentimentscores_day11),Sentimentscores_day11)
rownames(Sentimentscores_day11)<-NULL

saveRDS(Sentimentscores_day11, "day11.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day11,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 26th, 2017")
