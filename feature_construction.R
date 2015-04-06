#requires FSelector, mlbench
library(FSelector)
library(mlbench)

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
