#requires FSelector, mlbench
library(FSelector)
library(mlbench)

#
# extracts features from the document term matrix
# features extracted are mainly the frequent words in the received tweets
# input: 
#       dtm - a DocumentTermMatrix object
# returns: 
#       a data frame having just the terms from the feature set
#
extract.features = function(dtm) {
  # extract frequent terms  
  reservoir <- union(findFreqTerms(dtm, lowfreq = 10), c('love','hate'))
  
  # convert to data frame
  dtm.matrix <- as.matrix(dtm)
  dtm.df <- as.data.frame(dtm.matrix)
  
  # select only the terms in the reservoir
  dtm.sub <- dtm.df[,reservoir]
  return (dtm.sub)
}
