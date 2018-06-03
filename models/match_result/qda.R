library(MASS)

make.qda.model <- function(matches, features.for.predicting) {
  result.mapping = vector(mode="list", length=length(levels(matches$target)))
  for (i in 1:length(levels(matches$target))) {
    result.mapping[[i]] = levels(matches$target)[i]
  }

  features.for.predicting
  matches <- matches[,features.for.predicting]
  
  model.qda <- qda(target ~ ., data = matches)
  predictions.qda <- predict(model.qda, type="response")
  predictions <- predictions.qda$posterior
  
  predicted.results <- c()
  for (i in 1:dim(predictions)[1]) {
    # get max probability for predicting result
    predicted.value.index <- which(predictions[i,] == max(predictions[i,]))[1]
    
    # get string value for predicting result (e.g. "Home win")
    predicted.result <- result.mapping[[predicted.value.index]]
    
    # append the result to the results list
    predicted.results <- c(predicted.results, predicted.result)
  }
  prediction.accuracy.qda <- mean(matches$result == predicted.results)
  
  result = vector(mode="list", length=3)
  result[["model"]] <- model.qda
  result[["predictions"]] <- predicted.results
  result[["accuracy"]] <- prediction.accuracy.qda
  
  return(result)
}

# Example usage (requires strenght.home.team and strenght.away.team computed inside matches_csv)
# result <- make.qda.model(matches_csv)