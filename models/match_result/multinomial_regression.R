library(nnet)

make.multinomial.logistic.regression.model <- function(matches, features.for.predicting) {
  result.mapping = vector(mode="list", length=length(levels(matches$target)))
  for (i in 1:length(levels(matches$target))) {
    result.mapping[[i]] = levels(matches$target)[i]
  }

  matches <- matches[,features.for.predicting]
  
  model.multinom <- multinom(target ~ ., data = matches)
  
  predicted.results <- c()
  predictions <- predict(model.multinom, newdata=matches, "probs")
  for (i in 1:dim(predictions)[1]) {
    # get max probability for predicting result
    predicted.value.index <- which(predictions[i,] == max(predictions[i,]))[1]
    
    # get string value for predicting result (e.g. "Home win")
    predicted.result <- result.mapping[[predicted.value.index]]
    
    # append the result to the results list
    predicted.results <- c(predicted.results, predicted.result)
  }
  
  prediction.accuracy.multinomial <- mean(matches$target == predicted.results)
  
  result = vector(mode="list", length=3)
  result[["model"]] <- model.multinom
  result[["predictions"]] <- predicted.results
  result[["accuracy"]] <- prediction.accuracy.multinomial

  return(result)
}

# Example usage (requires strenght.home.team and strenght.away.team computed inside matches_csv)
# result <- make.multinomial.logistic.regression.model(matches_csv)