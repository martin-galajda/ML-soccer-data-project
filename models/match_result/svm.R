library(kernlab)

make.svm.model <- function(data, features.for.predicting, kernel, C=100, type="C-svc") {
  
  train.data <- data[, features.for.predicting[!features.for.predicting %in% c("target")]]
  print(colnames(train.data))

  model.svm <- ksvm(as.matrix(train.data), data$target, type=type, kernel=kernel, C=C, scaled=c())
  
  result = vector(mode="list", length=3)
  result[["model"]] <- model.svm
  result[["predictions"]] <- 0
  result[["accuracy"]] <- 0
  
  return(result)
}

create.svm.model.builder <- function(kernel, C, type = "C-svc") {
  make.svm.model.wih.given.params <- function(data, features.for.predicting) {
    return(make.svm.model(data, features.for.predicting, kernel, C, type))
  }
  
  return(make.svm.model.wih.given.params)
}