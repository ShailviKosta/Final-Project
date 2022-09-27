#load twitter library
library(rtweet)
library(httpuv)

#text-mining library
library(tidytext)
library(reshape2)
library(syuzhet)

#plotting and pipes
library(ggplot2)
library(dplyr)


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

