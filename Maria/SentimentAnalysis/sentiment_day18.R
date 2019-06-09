
#Getting all the tweets with geocoded information
allTweetsDay17<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-10-03' AND '2017-10-04';
")

residentTweetsDay17<- allTweetsDay17[allTweetsDay17$tweetuser %in% residentList,]

tweet_day17<- residentTweetsDay17$tweettext
#convert all text to lower case
tweet_day17<- tolower(tweet_day17)
# Replace blank space (“rt”)
tweet_day17 <- gsub("rt", "", tweet_day17)
# Replace @UserName
tweet_day17 <- gsub("@\\w+", "", tweet_day17)
# Remove punctuation
tweet_day17 <- gsub("[[:punct:]]", "", tweet_day17)
# Remove links
tweet_day17 <- gsub("http\\w+", "", tweet_day17)
# Remove tabs
tweet_day17 <- gsub("[ |\t]{2,}", "", tweet_day17)
# Remove blank spaces at the beginning
tweet_day17 <- gsub("^ ", "", tweet_day17)
# Remove blank spaces at the end
tweet_day17 <- gsub(" $", "", tweet_day17)

#getting emotions using in-built function
sentiment_day17<-get_nrc_sentiment((tweet_day17))
#calculationg total score for each sentiment
Sentimentscores_day17<-data.frame(colSums(sentiment_day17[,]))

names(Sentimentscores_day17)<-"Score"
Sentimentscores_day17<-cbind("day"=rep(c(33),10), "sentiment"=rownames(Sentimentscores_day17),Sentimentscores_day17)
rownames(Sentimentscores_day17)<-NULL

saveRDS(Sentimentscores_day17, "day18.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day17,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Oct 3rd, 2017")

