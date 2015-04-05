# requires RCurl
library("twitteR")

retrieve.tweets = function(n=500) {
  api_key = "JISW38mCBmDF7y2CcwbsGXUZQ"
  api_secret = "MyYgjlByQC4I2XNI9HyNKxaFWxsmtXwQ4vvMrmRquRnnyYCTOm"
  access_token = "2990620234-g3k7vo5EBkMkiUZg0jURmvTCMWLQADfNpbO0dnX"
  access_secret = "Tv3nwblAlu5YnM40L7LHXfjqhRVn41w4JbABrCz7kobBt"
  
  setup_twitter_oauth(api_key, api_secret, access_token, access_secret)
  
  tweets = searchTwitter('love OR hate', n, lang='en')
  #tweets = strip_retweets(tweets, strip_manual=TRUE, strip_mt=TRUE)
  tweets = twListToDF(tweets)
  return (tweets)
}