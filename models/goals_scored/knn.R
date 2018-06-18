library(e1071)
library(BBmisc)

### Prepare data and runs the model
run.knn.regression <- function () {
  
  # matches.merged.all.features = load.matches.with.all.features.for.match.result()
  
  # Select features for predicting
  features.for.keeping <- c(
    'result',
    'season',
    'attack.strength.home.team',
    'attack.strength.away.team',
    'win.ratio.home.team',
    'win.ratio.away.team',
    'win.ratio.home.team.playing.home',
    'win.ratio.home.team.playing.away',
    'win.ratio.away.team.playing.home',
    'win.ratio.away.team.playing.away'
  )
  
  # select from dataframe only columns relevant for predicting
  features.for.predicting <- features.for.keeping[3:length(features.for.keeping)]
  
  #normalize data and prepare home and away datasets
  matches.for.training.home <- normalize(matches.merged.all.features[,features.for.keeping])
  matches.for.training.home$target <- matches.merged.all.features$home_team_goal
  
  matches.for.training.away <- normalize(matches.merged.all.features[,features.for.keeping])
  matches.for.training.away$target <- matches.merged.all.features$away_team_goal
  
  features.for.predicting <- c(
    'target', # this needs to always be inside dataframe
    'attack.strength.home.team',
    'attack.strength.away.team',
    'win.ratio.home.team',
    'win.ratio.away.team',
    'win.ratio.home.team.playing.home',
    'win.ratio.away.team.playing.away'
  )
  
  #predicted.goals.home <- make.knn.model (matches.for.training.home, features.for.predicting, 0.1, 107)
  predicted.goals.home <- tune.knn.model (matches.for.training.home, features.for.predicting, 0.1, 100:200)
  predicted.goals.away <- c()
  
  return( c(predicted.goals.home, predicted.goals.away) )
}

### This function creates the model and predicts goals using the optimal parameter k (found in model tuning)
### depending on the column copied into "target" this function predicts the goals for the home or away team
### K can be set when calling the function, default is 1
make.knn.model <- function(matches, features.for.predicting, test.method, k = 1) {
  
  matches <- matches[,features.for.predicting]
  
  # spliting date in training and test data
  index.te = if (test.method == "last.season") which(matches$season == "2015/2016") else 
    sample(seq_len(nrow(matches)),size=nrow(matches)*test.method)
  
  matches.train = matches[-index.te,]
  matches.test = matches[index.te,]
  
  # create and train the model
  pred_001 = FNN::knn.reg(train = matches.train[,-which(names(matches.train)=="target")], test = matches.test[,-which(names(matches.test)=="target")], y = matches.train$target, k = k)
  
  predicted.goals <- pred_001$pred
  
  nrmse.knn <-  (mean((matches.test$target - predicted.goals)^2)/var(matches.test$target))^0.5
  accuracy.knn <- mean(matches.test$target == round(predicted.goals))
  

  print(cbind(matches.test$target, round(predicted.goals)))
  
  print(nrmse.knn)
  print(accuracy.knn)
  
  result = vector(mode="list")
  result[["model"]] <- pred_001
  result[["predictions"]] <- predicted.goals
  result[["nrmse"]] <- nrmse.knn
  result[["accuracy"]] <- accuracy.knn
  
  return (result) 
}

### This function tunes the models parameter k and runs the model afterwards
### depending on the column copied into "target" this function predicts the goals for the home or away team
### Additional to the values retunred by make.knn.model this function retunrs the best k found
tune.knn.model <- function(matches, features.for.predicting, test.method, k.values) {
  
  matches <- matches[,features.for.predicting]
  
  # spliting date in training and test data
  index.te = if (test.method == "last.season") which(matches$season == "2015/2016") else 
    sample(seq_len(nrow(matches)),size=nrow(matches)*test.method)
  
  matches.train = matches[-index.te,]
  matches.test = matches[index.te,]
  
  # create and train the model
  k.errors <- c()
  
  for(k in k.values){
    pred = FNN::knn.reg(train = matches.train[,-which(names(matches.train)=="target")], test = matches.test[,-which(names(matches.test)=="target")], y = matches.train$target, k = k)
    k.errors[k] <-  (mean((matches.test$target - pred$pred)^2)/var(matches.test$target))^0.5
    #k.errors[k] <-  mean(matches.test$target == round(pred$pred))
  }
  
  best.k <- which.min(k.errors)
  
  print(best.k)
  
  pred_001 = FNN::knn.reg(train = matches.train[,-which(names(matches.train)=="target")], test = matches.test[,-which(names(matches.test)=="target")], y = matches.train$target, k = best.k)
  
  predicted.goals <- pred_001$pred
  
  nrmse.knn <-  (mean((matches.test$target - predicted.goals)^2)/var(matches.test$target))^0.5
  accuracy.knn <- mean(matches.test$target == round(predicted.goals))
  
  
  #print(cbind(matches.test$target, round(predicted.goals)))
  
  print(nrmse.knn)
  print(accuracy.knn)
  
  result = vector(mode="list")
  result[["model"]] <- pred_001
  result[["predictions"]] <- predicted.goals
  result[["nrmse"]] <- nrmse.knn
  result[["accuracy"]] <- accuracy.knn
  result[["best_k"]] <- best.k
  
  return (result) 
}
