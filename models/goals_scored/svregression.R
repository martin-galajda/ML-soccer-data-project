library(e1071)

### Prepare data and runs the model - should be replaced by global feature cross validation
run.svr.regression <- function () {

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
  
  features.for.predicting <- features.for.keeping[3:length(features.for.keeping)]
  # select from dataframe only columns relevant for predicting
  
  # matches.merged.all.features is the return from load.matches.with.all.features.for.match.result() as in line 18 of run_models.R it contains all features including the win ratios 
  
  matches.for.training <- matches.merged.all.features[,features.for.keeping] # see line above ^
  matches.for.training$target <- matches.merged.all.features$home_team_goal
  
  features.for.predicting <- c(
    'target', # this needs to always be inside dataframe
    'attack.strength.home.team',
    'attack.strength.away.team',
    'win.ratio.home.team',
    'win.ratio.away.team',
    'win.ratio.home.team.playing.home',
    'win.ratio.away.team.playing.away'
  )
  
  return( make.svregression.model (matches.for.training, features.for.predicting) )
}

### Creates the model and predicts goals using the optimal parameters (found in previous model tuning)
make.svregression.model <- function(matches, features.for.predicting) {
  
  matches <- matches[,features.for.predicting]
  
  model.svr <- svm(target ~., data = matches, epsilon = 0.6, cost=2)
 
  predicted.goals <- predict(model.svr, data = matches)
  
  rmse.svr <- sqrt((matches$target - predicted.goals)^2)
  
  result = vector(mode="list", length=3)
  result[["model"]] <- model.svr.tuned
  result[["predictions"]] <- predicted.goals.tuned
  result[["accuracy"]] <- rmse.svr.tuned
   
  return (result) 
}

### Tuning the model will take a long time (depending on the parameters it took me 10 to 30 hours)
tune.svregression.model <- function(matches, features.for.predicting) {

  matches <- matches[,features.for.predicting]

  tuneResult <- tune(svm, target ~., data = matches,
                     ranges = list(epsilon = seq(0,1,0.2), cost = 2^(1:5))
  )
  
  print(tuneResult)
  
  model.svr.tuned <- tuneResult$best.model
  
  predicted.goals.tuned <- predict(tunedModel, data = matches) 
  
  rmse.svr.tuned <-  sqrt((matches$target - predictions.optimized)^2)
  
  result = vector(mode="list", length=3)
  result[["model"]] <- model.svr.tuned
  result[["predictions"]] <- predicted.goals.tuned
  result[["accuracy"]] <- rmse.svr.tuned
  
  return (result) 
}
