#load twitter library
library(rtweet)
library(httpuv)
library(magrittr)


#text-mining library
library(tidytext)
library(reshape2)
library(forestmangr)
library(syuzhet)

#plotting and pipes
library(ggplot2)
library(dplyr)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

#Store api keys
api_key<- "dC8GNmNIxE5HCLAsy5Q248sdl"
api_secret_key<- "JTBUlRHN5NSRel58ITdxm3eXKjTptxyNm418OeeCZKgDHZMxOw"
access_token<- "1542167549231919104-ISwWZ9RR4zBL5WT6N05cyPwV6AbsgZ"
access_secret_key<- "q1toBHKae4lICcdeSPSjCAk23tsCMYTaiocm05m48tJtZ"

#authenticate via web browser

token<-create_token(
  app = "S_Practice3",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_secret_key)

token

#MarvelStudios

# Remove retweets
Marvel_tweets <- get_timeline("@MarvelStudios", n= 3200)
Marvel_tweets_organic <- Marvel_tweets[Marvel_tweets$is_retweet==FALSE, ]
head(Marvel_tweets_organic)

# Remove replies
Marvel_tweets_organic <- subset(Marvel_tweets_organic, 
                                is.na(Marvel_tweets_organic$reply_to_status_id))

Marvel_tweets_organic <- Marvel_tweets_organic %>% arrange(-favorite_count)
Marvel_tweets_organic[1,5]

Marvel_tweets_organic <- Marvel_tweets_organic %>% arrange(-retweet_count)
Marvel_tweets_organic[1,5]

# Keeping only the retweets
Marvel_retweets <- Marvel_tweets[Marvel_tweets$is_retweet==TRUE,]

# Keeping only the replies
Marvel_replies <- subset(Marvel_tweets, !is.na(Marvel_tweets$reply_to_status_id))

# Creating a data frame
data_marvel <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  countmarvel=c(2205, 587, 396)
)

# Adding columns 
data_marvel$fractionmarvel = data_marvel$countmarvel / sum(data_marvel$countmarvel)
data_marvel$percentagemarvel = data_marvel$countmarvel / sum(data_marvel$countmarvel) * 100
data_marvel$ymaxmarvel = cumsum(data_marvel$fractionmarvel)
data_marvel$yminmarvel = c(0, head(data_marvel$ymaxmarvel, n=-1))

# Rounding the data to two decimal points
data_marvel <- round_df(data_marvel, 2)

# Specify what the legend should say
Type_of_Tweet_marvel <- paste(data_marvel$category, data_marvel$percentagemarvel, "%")
ggplot(data_marvel, aes(ymax=ymaxmarvel, ymin=yminmarvel, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() + ggtitle("MarvelStudios")+
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),legend.position = "right")


#DCComics

# Remove retweets
DC_tweets <- get_timeline("@DCComics", n= 3200)
DC_tweets_organic <- DC_tweets[DC_tweets$is_retweet==FALSE, ]

# Remove replies
DC_tweets_organic <- subset(DC_tweets_organic, 
                                is.na(DC_tweets_organic$reply_to_status_id))

DC_tweets_organic <- DC_tweets_organic %>% arrange(-favorite_count)
DC_tweets_organic[1,5]

DC_tweets_organic <- DC_tweets_organic %>% arrange(-retweet_count)
DC_tweets_organic[1,5]

# Keeping only the retweets
DC_retweets <- DC_tweets[DC_tweets$is_retweet==TRUE,]

# Keeping only the replies
DC_replies <- subset(DC_tweets, !is.na(DC_tweets$reply_to_status_id))

# Creating a data frame
data_dc <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  countdc=c(1146, 785, 1258)
)

# Adding columns 
data_dc$fractiondc = data_dc$countdc / sum(data_dc$countdc)
data_dc$percentagedc = data_dc$countdc / sum(data_dc$countdc) * 100
data_dc$ymaxdc = cumsum(data_dc$fractiondc)
data_dc$ymindc = c(0, head(data_dc$ymaxdc, n=-1))

# Rounding the data to two decimal points
data_dc <- round_df(data_dc, 2)

# Specify what the legend should say
Type_of_Tweet_dc <- paste(data_dc$category, data_dc$percentagedc, "%")
ggplot(data_dc, aes(ymax=ymaxdc, ymin=ymindc, xmax=4, xmin=3, 
                    fill=Type_of_Tweet_dc)) +
  geom_rect() + ggtitle("DC Comics")+
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5),legend.position = "right")


##Source of Tweets Published

##Marvel Studios
Marvel_app <- Marvel_tweets %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
Marvel_app <- subset(Marvel_app, count > 11)

source_marvel <- data.frame(category=Marvel_app$source,
                            count_marvel= Marvel_app$count)

source_marvel$fraction_marvel = source_marvel$count_marvel / sum(source_marvel$count_marvel)
source_marvel$percentage_marvel = source_marvel$count_marvel/ sum(source_marvel$count_marvel) * 100
source_marvel$ymax_marvel = cumsum(source_marvel$fraction_marvel)
source_marvel$ymin_marvel = c(0, head(source_marvel$ymax_marvel, n=-1))
source_marvel <- round_df(source_marvel, 2)
Types_of_Sources <- paste(source_marvel$category, source_marvel$percentage_marvel, "%")
ggplot(source_marvel, aes(ymax=ymax_marvel, ymin=ymin_marvel, xmax=4, xmin=3, 
                           fill=Types_of_Sources)) +
  geom_rect() + ggtitle("Marvel Studios Sources")+
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(plot.title= element_text(hjust=0.5),legend.position = "right")


##DC Comics

DC_app <- DC_tweets %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
DC_app <- subset(DC_app, count > 11)

source_dc <- data.frame(
  category=DC_app$source,
  count_dc= DC_app$count
)

source_dc$fraction_dc = source_dc$count_dc / sum(source_dc$count_dc)
source_dc$percentage_dc = source_dc$count_dc/ sum(source_dc$count_dc) * 100
source_dc$ymax_dc = cumsum(source_dc$fraction_dc)
source_dc$ymin_dc = c(0, head(source_dc$ymax_dc, n=-1))
source_dc <- round_df(source_dc, 2)
Types_of_Sources <- paste(source_dc$category, source_dc$percentage_dc, "%")
ggplot(source_dc, aes(ymax=ymax_dc, ymin=ymin_dc, xmax=4, xmin=3, 
                          fill=Types_of_Sources)) +
  geom_rect() + ggtitle("DC Comics Sources")+
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(plot.title= element_text(hjust=0.5),legend.position = "right")

##Most Used Hashtags

#Marvel

Marvel_tweets_organic$hashtags <- as.character(Marvel_tweets_organic$hashtags)
Marvel_tweets_organic$hashtags <- gsub("c\\(", "", Marvel_tweets_organic$hashtags)
set.seed(1234)
wordcloud(Marvel_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#DCComics

DC_tweets_organic$hashtags <- as.character(DC_tweets_organic$hashtags)
DC_tweets_organic$hashtags <- gsub("c\\(", "", DC_tweets_organic$hashtags)
set.seed(1234)
wordcloud(DC_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


##Sentiment Analysis

#Marvel

Marvel_tweets <- Marvel_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
Marvel_tweets <- Marvel_tweets %>%
  anti_join(stop_words)

# Converting tweets to ASCII to trackle strange characters
Marvel_tweets <- iconv(Marvel_tweets, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
Marvel_tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",Marvel_tweets)
# removing mentions, in case needed
Marvel_tweets <-gsub("@\\w+","",Marvel_tweets)
ew_sentiment<-get_nrc_sentiment((Marvel_tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none",)+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on Scores_Marvel")+
  theme(plot.title = element_text(hjust = 0.5))


#DCComics

DC_tweets <- DC_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
DC_tweets <- DC_tweets %>%
  anti_join(stop_words)

# Converting tweets to ASCII to trackle strange characters
DC_tweets <- iconv(DC_tweets, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
DC_tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",DC_tweets)
# removing mentions, in case needed
DC_tweets <-gsub("@\\w+","",DC_tweets)
ew_sentiment<-get_nrc_sentiment((DC_tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none",)+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on Scores_DCCOMICS")+
  theme(plot.title = element_text(hjust = 0.5))
