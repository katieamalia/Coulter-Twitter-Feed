

#Katie Serrano

#2/27/19



#install packages

install.packages("rio") 
install.packages("tidyverse")
install.packages("readxl")
install.packages("dplyr")
install.packages("janitor")
install.packages("tidyr")
install.packages("tidytext")
install.packages("kableExtra")
install.packages("ggplot2")
install.packages("tidyr")


library(tidytext)
library(tidyr)
library(stringr)
library (tidyverse)
library (readxl)
library (dplyr)
library(rio)
library(janitor)
library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)



#import data

rio::import("Coulter.csv")

#make R friendly

Coulter <- janitor::clean_names(Coulter)

#Common Words

tweet_words <- Coulter %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
          str_detect(word, "[a-z]"))



CommonCoulter <- tweet_words %>%
  count(word, source) %>%
  filter(sum(n) >= 10) %>%
  spread(source, n, fill = 0) %>%
  ungroup()



CommonCoulter <- tweet_words %>%
  count(word) %>%
  filter(sum(n) >= 5) %>%
  ungroup()


CommonWord <- filter(CommonCoulter2, n >= 120) %>% 
  group_by(word, n) %>% 
  arrange(desc(n)) 

CommonWord2 <- filter(CommonCoulter2, n >= 120)

CommonCoulterWordChart <- ggplot(CommonWord2, aes(x = reorder(word, -n), y = n, color = word, fill=word))  +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Words In Coulter Twitter Feed", 
       subtitle = "Coulter Twitter Feed",
       caption = "Graphic by Katie Serrano",
       x="Word",
       y="Count of the word usage") +
  theme(legend.position="none") 
plot(CommonCoulterWordChart)



#Done!





#Tweets by Month



install.packages("lubridate")
install.packages("glue", type="mac.binary")
install.packages("stringi", type="mac.binary")
install.packages("stringr", type="mac.binary")
install.packages("lubridate", type="mac.binary")
install.packages("stringr")

library(lubridate)

#Create appropriate tables

Coulterdates <- Coulter %>% select(created_at, text, hashtags)

str(Coulterdates)
head(Coulterdates)

library(dplyr)
library(tidyr)
library(stringr)


Coulterdates2 <- Coulterdates %>% 
  separate(created_at, c("date", "seconds"), " ")

Coulterdates2$date2 <- ymd(Coulterdates2$date)

Coulterdates2$year <- year(Coulterdates2$date2)
Coulterdates2$month <- month(Coulterdates2$date2, label=TRUE)
Coulterdates2$day <- day(Coulterdates2$date2)
Coulterdates2$weekday <- wday(Coulterdates2$date2, label=TRUE, abbr=FALSE)

CoulterDaytotal <- Coulterdates2 %>% 
  count(date) %>% 
  group_by(date) %>% 
  arrange(desc(n))

Coultermonth <- Coulterdates2 %>% 
  count(month) %>% 
  group_by(month) %>% 
  arrange(desc(n))

#Now Plot

library(ggplot2)

#Mutate to correct yearly organization 

Coulteryear_mo <- Coulterdates2 %>%
  mutate(year = format(date2, "%Y"), yearmon = format(date2, "%Y-%m")) %>%
  group_by(year, yearmon) %>%
  count(yearmon)

Year_plot <- ggplot(Coulteryear_mo, aes(x=yearmon, y=n, color=yearmon, fill=yearmon))+
  geom_col(show.legend=FALSE)+
  labs(title="Coulter Monthly Twitter Activity", x="month", y="tweets", caption= "Source:Twitter")+
  theme_bw()
plot(Year_plot)

#Done



#Now on to hashtags

library(dplyr)
library(tidyr)


Coulter2 <- select(Coulter, user_id, created_at, text, is_retweet, hashtags, urls_expanded_url)
View(Coulter2)

hashtags <- Coulter %>% count(hashtags, sort=TRUE)

#clean up the junk

Coulter2$hashtag1 <- gsub("\\(", "", Coulter2$hashtags) 
Coulter2$hashtag1 <- gsub ("\\)", "", Coulter2$hashtag1)
Coulter2$hashtag1 <- gsub ("\"", "&", Coulter2$hashtag1)
Coulter2$hashtag1 <- gsub ("c&", "", Coulter2$hashtag1)
Coulter2$hashtag1 <- gsub ("c&&", "", Coulter2$hashtag1)
Coulter2$hashtag1 <- gsub ("&&", "", Coulter2$hashtag1)
Coulter2$hashtag1 <- gsub ("&", "", Coulter2$hashtag1)

#Separate the hashtags

Coulter3 <- separate(Coulter2, hashtag1, 
                 c('hashtag1', 'hashtag2', 'hashtag3', 'hashtag4', 'hashtag5'), 
                 sep=',', remove=TRUE)

#make a table with only hashtags
Coulter4 <- select(Coulter3, hashtag1:hashtag5)

count <- table(unlist(Coulter4))
hashtag2 <- as.data.frame(count)

#rename columns
colnames(hashtag2)[1] <- "hashtag"
colnames(hashtag2)[2] <- "count"

Coulter5 <- hashtag2 %>%
  separate_rows(hashtag, sep = ' ') %>%
  group_by(hashtag = tolower(hashtag)) %>%
  summarise(Count = n(), 
            ScoreSum = sum(count))
hashtags3<- select(Coulter5, hashtag, ScoreSum) %>% arrange(desc(ScoreSum))

#filter

hashtags4<- hashtags3 %>% select(hashtag, ScoreSum) %>% filter(ScoreSum<67, ScoreSum>2)

#Chart it

CommonHashtags <- ggplot(hashtags4, aes(x = reorder(hashtag, ScoreSum), y = ScoreSum, color = hashtag, fill=hashtag))  +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Hashtags in Coulter Twitter Feed", 
       subtitle = "Coulter Twitter Feed",
       caption = "Graphic by Katie Serrano",
       x="Hashtag",
       y="Count of hashtag usage")
plot(CommonHashtags)

#done

