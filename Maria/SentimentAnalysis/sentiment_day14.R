#Getting all the tweets with geocoded information
allTweetsDay14<-dbGetQuery(conn,
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
WHERE tweetcreated BETWEEN '2017-09-29' AND '2017-09-30';
")

residentTweetsDay14<- allTweetsDay14[allTweetsDay14$tweetuser %in% residentList,]

tweet_day14<- residentTweetsDay14$tweettext
#convert all text to lower case
tweet_day14<- tolower(tweet_day14)
# Replace blank space (“rt”)
tweet_day14 <- gsub("rt", "", tweet_day14)
# Replace @UserName
tweet_day14 <- gsub("@\\w+", "", tweet_day14)
# Remove punctuation
tweet_day14 <- gsub("[[:punct:]]", "", tweet_day14)
# Remove links
tweet_day14 <- gsub("http\\w+", "", tweet_day14)
# Remove tabs
tweet_day14 <- gsub("[ |\t]{2,}", "", tweet_day14)
# Remove blank spaces at the beginning
tweet_day14 <- gsub("^ ", "", tweet_day14)
# Remove blank spaces at the end
tweet_day14 <- gsub(" $", "", tweet_day14)

#getting emotions using in-built function
sentiment_day14<-get_nrc_sentiment((tweet_day14))
#calculationg total score for each sentiment
Sentimentscores_day14<-data.frame(colSums(sentiment_day14[,]))

names(Sentimentscores_day14)<-"Score"
Sentimentscores_day14<-cbind("day"=rep(c(29),10), "sentiment"=rownames(Sentimentscores_day14),Sentimentscores_day14)
rownames(Sentimentscores_day14)<-NULL

saveRDS(Sentimentscores_day14, "day14.Rds")

#plotting the sentiments with scores
ggplot(data=Sentimentscores_day14,aes(x=sentiment,y=Score, group=1))+ geom_line()+
  geom_point()+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people on Sep 29th, 2017")

