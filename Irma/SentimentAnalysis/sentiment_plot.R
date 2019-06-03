library(ggplot2)
library(dplyr)

day1 <- readRDS("day1.Rds")
day2 <- readRDS("day2.Rds")
day3 <- readRDS("day3.Rds")
day4 <- readRDS("day4.Rds")
day5 <- readRDS("day5.Rds")
day6 <- readRDS("day6.Rds")
day7 <- readRDS("day7.Rds")
day8 <- readRDS("day8.Rds")
day9 <- readRDS("day9.Rds")
day10 <- readRDS("day10.Rds")

new <- rbind(day1, day2)
new <- rbind(new, day3)
new <- rbind(new, day4)
new <- rbind(new, day5)
new <- rbind(new, day6)
new <- rbind(new, day7)
new <- rbind(new, day8)
new <- rbind(new, day9)
new <- rbind(new, day10)

head(new)

new$day <- as.factor(new$day)

#plotting the sentiments with scores
p <- ggplot(data=new,aes(x=sentiment,y=Score, group=day))+ geom_line(aes(color=day))+
  geom_point(aes(color=day))+
  theme(legend.position="right")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people during Hurricane Irma (Day 1 = Sept 4: Day 10 = Sept 13)")

#p+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#FF6347", "#228B22", "#008080", "#FFA500", "#800080", "#CCCC00", "#FF69B4"))
p+scale_color_brewer(palette="Paired")

positive <- subset(new, sentiment=="positive")

head(positive)

negative <- subset(new, sentiment=="negative")
head(negative)

fear <- subset(new, sentiment=="fear")

new2 <- rbind(positive, negative)
new2 <- rbind(new2, fear)

q <- ggplot(data=new2,aes(x=day,y=Score, group=sentiment))+ geom_line(aes(color=sentiment))+
  geom_point(aes(color=sentiment))+
  theme(legend.position="right")+
  xlab("Day")+ylab("scores")+ggtitle("Sentiments of people during Hurricane Irma (Day 1 = Sept 4: Day 10 = Sept 13)")


q+scale_color_brewer(palette="Dark2")
