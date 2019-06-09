
#Getting all the tweets with geocoded information
allTweetsDay3<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-09-18' AND '2017-09-19';
")

residentTweetsDay3<- allTweetsDay3[allTweetsDay3$tweetuser %in% residentList,]

tweet_day3<- residentTweetsDay3$tweettext
#convert all text to lower case
tweet_day3<- tolower(tweet_day3)
# Replace blank space (“rt”)
tweet_day3 <- gsub("rt", "", tweet_day3)
# Replace @UserName
tweet_day3 <- gsub("@\\w+", "", tweet_day3)
# Remove punctuation
tweet_day3 <- gsub("[[:punct:]]", "", tweet_day3)
# Remove links
tweet_day3 <- gsub("http\\w+", "", tweet_day3)
# Remove tabs
tweet_day3 <- gsub("[ |\t]{2,}", "", tweet_day3)
# Remove blank spaces at the beginning
tweet_day3 <- gsub("^ ", "", tweet_day3)
# Remove blank spaces at the end
tweet_day3 <- gsub(" $", "", tweet_day3)

#getting emotions using in-built function
sentiment_day3<-get_nrc_sentiment((tweet_day3))
#calculationg total score for each sentiment
Sentimentscores_day3<-data.frame(colSums(sentiment_day3[,]))

names(Sentimentscores_day3)<-"Score"
Sentimentscores_day3<-cbind("day"=rep(c(18),10), "sentiment"=rownames(Sentimentscores_day3),Sentimentscores_day3)
rownames(Sentimentscores_day3)<-NULL

saveRDS(Sentimentscores_day3, "day3.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day3,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 18th, 2017")
