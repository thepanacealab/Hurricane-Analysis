
#Getting all the tweets with geocoded information
allTweetsDay9<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-09-24' AND '2017-09-25';
")

residentTweetsDay9<- allTweetsDay9[allTweetsDay9$tweetuser %in% residentList,]

tweet_day9<- residentTweetsDay9$tweettext
#convert all text to lower case
tweet_day9<- tolower(tweet_day9)
# Replace blank space (“rt”)
tweet_day9 <- gsub("rt", "", tweet_day9)
# Replace @UserName
tweet_day9 <- gsub("@\\w+", "", tweet_day9)
# Remove punctuation
tweet_day9 <- gsub("[[:punct:]]", "", tweet_day9)
# Remove links
tweet_day9 <- gsub("http\\w+", "", tweet_day9)
# Remove tabs
tweet_day9 <- gsub("[ |\t]{2,}", "", tweet_day9)
# Remove blank spaces at the beginning
tweet_day9 <- gsub("^ ", "", tweet_day9)
# Remove blank spaces at the end
tweet_day9 <- gsub(" $", "", tweet_day9)

#getting emotions using in-built function
sentiment_day9<-get_nrc_sentiment((tweet_day9))
#calculationg total score for each sentiment
Sentimentscores_day9<-data.frame(colSums(sentiment_day9[,]))

names(Sentimentscores_day9)<-"Score"
Sentimentscores_day9<-cbind("day"=rep(c(24),10), "sentiment"=rownames(Sentimentscores_day9),Sentimentscores_day9)
rownames(Sentimentscores_day9)<-NULL

saveRDS(Sentimentscores_day9, "day9.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day9,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 24th, 2017")
