#library(RMOA)
library(kernlab)

trainModel <- function(tdm) {
  #hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
  #tdm.fact <- factorise(tdm)
  #tdm.stream <- datastream_dataframe(tdm.fact)
  #model <- trainMOA(model = hdt, formula = Class ~ ., data = tdm.stream)
  #return (model)
  modelSVM <- ksvm(x=as.matrix(tdm[,-ncol(tdm)]), y=tdm[,ncol(tdm)])
  return (modelSVM)
}