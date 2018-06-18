library(e1071)

### Prepare data and runs the model
run.svr.regression <- function () {

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
  
  matches.for.training.home <- matches.merged.all.features[,features.for.keeping]
  matches.for.training.home$target <- matches.merged.all.features$home_team_goal
  
  matches.for.training.away <- matches.merged.all.features[,features.for.keeping]
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
  
  predicted.goals.home <- make.svregression.model (matches.for.training.home, features.for.predicting, 0.1)
  predicted.goals.away <- make.svregression.model (matches.for.training.away, features.for.predicting, 0.1)
  
  return( c(predicted.goals.home, predicted.goals.away) )
}

### This function creates the model and predicts goals using the optimal parameters (found in previous model tuning)
### depending on the column copied into "target" this function predicts the goals for the home or away team
make.svregression.model <- function(matches, features.for.predicting, test.method) {
  
  matches <- matches[,features.for.predicting]
  
  # spliting date in training and test data
  index.te = if (test.method == "last.season") which(matches$season == "2015/2016") else 
    sample(seq_len(nrow(matches)),size=nrow(matches)*test.method)
  
  matches.train = matches[-index.te,]
  matches.test = matches[index.te,]
  
  # create and train the model
  model.svr <- svm(target ~., data = matches.train, epsilon = 0.6, cost=2)
 
  # predict goals
  predicted.goals <- predict(model.svr, data = matches.test)
  
  # calculate error end accuracy
  
  nrmse.svr <-  (mean((matches.test$target - predicted.goals)^2)/var(matches.test$target))^0.5
  accuracy.svr <- mean(matches.test$target == round(predicted.goals))
  
  print(nrmse.svr)
  print(accuracy.svr)
  
  result = vector(mode="list")
  result[["model"]] <- model.svr
  result[["predictions"]] <- predicted.goals
  result[["nrmse"]] <- nrmse.svr
  result[["accuracy"]] <- accuracy.svr
   
  return (result) 
}

### This function tunes the models parameter (cost and epsilon) and runs the model afterwards
### depending on the column copied into "target" this function predicts the goals for the home or away team
### The tuning of the model might take a long time (depending on the parameters it took me 10 to 30 hours)
tune.svregression.model <- function(matches, features.for.predicting, test.method) {

  matches <- matches[,features.for.predicting]
  
  # spliting date in training and test data
  index.te = if (test.method == "last.season") which(matches$season == "2015/2016") else 
    sample(seq_len(nrow(matches)),size=nrow(matches)*test.method)
  
  matches.train = matches[-index.te,]
  matches.test = matches[index.te,]
  
  # tuning of the model
  tuneResult <- tune(svm, target ~., data = matches.train,
                     ranges = list(epsilon = seq(0,1,0.2), cost = 2^(1:5))
  )
  
  print(tuneResult)
  
  model.svr <- tuneResult$best.model
  
  predicted.goals <- predict(tunedModel, data = matches.test) 
  
  nrmse.svr <-  (mean((matches.test$target - predicted.goals)^2)/var(matches.test$target))^0.5
  accuracy.svr <- mean(matches.test$target == round(predicted.goals))
  
  print(nrmse.svr)
  print(accuracy.svr)
  
  result = vector(mode="list")
  result[["model"]] <- model.svr
  result[["predictions"]] <- predicted.goals
  result[["nrmse"]] <- nrmse.svr
  result[["accuracy"]] <- accuracy.svr
  
  return (result) 
}
