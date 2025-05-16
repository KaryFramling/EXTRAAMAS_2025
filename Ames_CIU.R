library(AmesHousing)
library(caret)
library(gbm)
library(ciu)

caret.model="gbm"

# We don't care about test set etc here, we trust in k-fold in this case.
trainData <- sxai.data
# trainIdx <- createDataPartition(data[,target], p=0.8, list=FALSE)
# trainData = data[trainIdx,]
# testData = data[-trainIdx,]

# Train and show some performance indicators.
kfoldcv <- trainControl(method="cv", number=10)
sxai.ai.model <- train(Sale_Price~., trainData, method=caret.model, trControl=kfoldcv)

# Most expensive instances
expensive <- which(sxai.data$Sale_Price>500000)
# Cheapest instances
cheap <- which(sxai.data$Sale_Price<50000)

# Explanations
Ames_voc <- ciu.voc.from.graph(sxai.graph,colnames(sxai.data)[colnames(sxai.data) != "Sale_Price"])
sxai.ciu <- ciu.new(sxai.ai.model, Sale_Price~., trainData, vocabulary = Ames_voc)
for ( inst.ind in c(expensive,cheap)[1:min(3,length(c(expensive,cheap)))] ) {
  instance <- subset(sxai.data[inst.ind,], select=-Sale_Price)
  print(sxai.ciu$ggplot.col.ciu(instance, sort="CI", plot.mode="overlap"))
  print(sxai.ciu$ggplot.ciu(instance,46))
  print(sxai.ciu$ggplot.ciu(instance,78))
}

