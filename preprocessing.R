library(tm)
# requires SnowballC
process.tweets = function(vc_tweets) {
  vc_tweets = tm_map(vc_tweets, content_transformer(tolower), lazy = TRUE)
  vc_tweets = tm_map(vc_tweets, removePunctuation)
  vc_tweets = tm_map(vc_tweets, removeNumbers)
  vc_tweets <- tm_map(vc_tweets, removeWords, stopwords("english"))
  vc_tweets <- tm_map(vc_tweets, stemDocument)
  return (vc_tweets)
}