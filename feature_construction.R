#requires FSelector, mlbench
library(FSelector)
library(mlbench)

gl.reservoir <- NULL

create.tdm = function(vc_tweets, training = F) {
  tdm <- TermDocumentMatrix(vc_tweets, control=list(minWordLength=1))
  if(training) {
    gl.reservoir <<- union(findFreqTerms(tdm, 5, 100), c('love','hate'))
    reservoir <- gl.reservoir
  }
  else {
    #reservoir <- intersect(gl.reservoir, findFreqTerms(tdm))
    reservoir <- findFreqTerms(tdm)[1:length(gl.reservoir)]
    if(!('love' %in% reservoir)) {
      reservoir[1] <- 'love'
    }
    if(!('hate' %in% reservoir)) {
      reservoir[2] <- 'hate'
    }
  }
  reservoir <- union(reservoir, c('love','hate'))
  tdm.matrix <- as.matrix(tdm)
  tdm.df <- as.data.frame(tdm.matrix)
  tdm.sub <- tdm.df[reservoir,] # select only the terms in the reservoir
  return (tdm.sub)
  #ggplot(tdm.sub, aes("127", "144")) +
  #geom_text(label = rownames(tdm.sub), 
   #         position=position_jitter())
}