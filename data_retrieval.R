# requires RCurl
library("twitteR")

#
# function to get tweets having words 'love' OR 'hate' from twitter stream
# initial execution of the function requires to set authentication manually
# input: 
#       n - number of tweets to fetch
# returns: 
#       a data frame containing tweets received from twitter
#
retrieve.tweets = function(n=500) {
  api_key = "JISW38mCBmDF7y2CcwbsGXUZQ"
  api_secret = "MyYgjlByQC4I2XNI9HyNKxaFWxsmtXwQ4vvMrmRquRnnyYCTOm"
  access_token = "2990620234-g3k7vo5EBkMkiUZg0jURmvTCMWLQADfNpbO0dnX"
  access_secret = "Tv3nwblAlu5YnM40L7LHXfjqhRVn41w4JbABrCz7kobBt"
  
  # setup twitter authentication
  setup_twitter_oauth(api_key, api_secret, access_token, access_secret)
  
  # get tweets
  tweets = searchTwitter('love OR hate', n, lang='en')
  
  # convert to data frame
  tweets = twListToDF(tweets)
  return (tweets)
}