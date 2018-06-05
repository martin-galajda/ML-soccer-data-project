library(class)

# create a function that returns an object of class knnClassifier
knnClassifier = function(trainingData, k = 10, features.for.predicting) {
  model = structure(list(x = trainingData[, -which(names(trainingData) %in% c("target"))], 
                         target = trainingData$target, 
                         k=k, 
                         features.for.predicting=features.for.predicting[features.for.predicting != "target"]), 
                    class = "knnClassifier") 
  return(model)
}

# create a method for function print for class knnClassifier
predict.knnClassifier <- function(modelObject, newdata = NULL, type) {
  print(modelObject$features.for.predicting)
  if (is.null(newdata)) {
    test.data = modelObject$x
    test <- test.data[, modelObject$features.for.predicting]
  } else {
    test.data <- newdata[, -which(names(newdata) %in% c("target"))]
    test <- test.data[, modelObject$features.for.predicting]
  }

  knn.preds <- knn (modelObject$x, test, modelObject$target, k=modelObject$k)
  
  return(knn.preds)
}

make.knn.model = function(data, features.for.predicting, k = 3) {
  train.data <- data[,features.for.predicting]
  model.knn <- knnClassifier(train.data, k, features.for.predicting = features.for.predicting)
  
  result = vector(mode="list", length=3)
  result[["model"]] <- model.knn
  result[["predictions"]] <- c()
  result[["accuracy"]] <- 0
  
  return(result)
}

create.knn.model.builder <- function(k) {
  make.knn.model.for.given.k <- function(data, features.for.predicting) {
    return(make.knn.model(data, features.for.predicting, k = k))
  }
  
  return(make.knn.model.for.given.k)
}
