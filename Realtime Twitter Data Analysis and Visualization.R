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
api_key<- "API_KEY"
api_secret_key<- "API_SECRET_KEY"
access_token<- "ACCESS_TOKEN"
access_secret_key<- "ACCESS_SECRET_TOKEN"

#authenticate via web browser

token<-create_token(
  app = "ABC",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_secret_key)

token

##Follower_Counts

users_id <- c("MarvelStudios", "DCComics")
users <- lookup_users(users_id, parse = TRUE, token = NULL)

user_df1<- users[,c("screen_name","followers_count")]
print(user_df1)

ggplot(data = user_df1, aes(x = screen_name, y= followers_count))+
geom_bar(stat = 'identity', color = c('red','blue'), fill = 'black',
         width = 0.40)


##Get_Timeline

#Marvel
get_marvel <- get_timeline("@MarvelStudios",n = 1000, include_rts = FALSE)
head(get_marvel[,1:5], 10)

#DCComics
get_dc <- get_timeline("@DCComics", n = 1000, include_rts = FALSE)
head(get_dc[,1:5], 10)


##Search and extract tweets

#Marvel
twts_marvel <- search_tweets("#Marvel", n = 1000, include_rts = TRUE, 
                             lang = "en")
head(twts_marvel[,1:5], 10)

#DCComics
twts_dc <- search_tweets("#DCComics", n =1000, include_rts = TRUE, lang = "en")
head(twts_dc[,1:5], 10)


##Retweets_Count 

#Marvel
tweets_marvel <- search_tweets("#MarvelStudios", n = 1000, lang = "en")
rtwt_marvel <-tweets_marvel[,c("text", "retweet_count")]
rtwt_marvel_sort <- arrange(rtwt_marvel, desc(retweet_count))
rtwt_unique_marvel <- unique(rtwt_marvel_sort, by = "text")
rownames(rtwt_unique_marvel) <- NULL
head(rtwt_unique_marvel)

#DCCOmics
tweets_dc <- search_tweets("#DCCOmics", n = 1000, lang = "en")
rtwt_dc <-tweets_dc[,c("text", "retweet_count")]
rtwt_dc_sort <- arrange(rtwt_dc, desc(retweet_count))
rtwt_unique_dc <- unique(rtwt_dc_sort, by = "text")
rownames(rtwt_unique_dc) <- NULL
head(rtwt_unique_dc)


##Filter Based on Tweet Popularity

#Marvel
tweets_pop_marvel <- search_tweets(
  "#MarvelStudios min_retweets:100 AND min_faves:100")
counts_marvel <- tweets_pop_marvel[c("text","retweet_count", "favorite_count")]
count_marvel1<-arrange(counts_marvel, desc(retweet_count))
head(count_marvel1)

#DCCOmics
tweets_pop_dc <- search_tweets(
  "DCCOmics min_retweets:100 AND min_faves:100")
counts_dc <- tweets_pop_dc[c("text","retweet_count", "favorite_count")]
count_dc1<-arrange(counts_dc, desc(retweet_count))
head(count_dc1)

#Frequency of Tweets

#Marvel
marvel_twts <- search_tweets("#MarvelStudios", n = 3000, include_rts = FALSE)
marvel_ts <- ts_data(marvel_twts, by ='hours')
ts_plot(marvel_twts, by = "hours", color = "red")

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


#DCComics
dc_twts <- search_tweets("#DCComics", n = 3000, include_rts = FALSE)
dc_ts <- ts_data(dc_twts, by ='hours')
ts_plot(dc_twts, by = "hours", color = "blue")

##Comparing Both Together

merged_df <- merge(marvel_ts, dc_ts, by = "time", all = TRUE)
melt_df <- melt(merged_df, na.rm = TRUE, id.vars = "time")
head(melt_df)

ggplot(data = melt_df, aes(x = time, y = value, col = variable))+
  geom_line(lwd = 0.72)+
  scale_color_manual(labels = c("Marvel", "DC"), values = c("red", "blue"))

