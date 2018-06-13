source('./loaders/load_matches_with_all_relevant_features_for_match_result.R')
source('./models/match_result/knn.R')
source('./validation/determine_best_feature_subset.R')
source('./validation/cross_validate.R')
matches.merged.all.features <- load.matches.with.all.features.for.match.result()

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
matches.for.training <- matches.merged.all.features[,features.for.keeping]
matches.for.training$target <- matches.for.training$result
matches.for.training <- matches.for.training[,c(features.for.predicting, 'target')]

# Determine best subset of features
results <- determine.best.subset.of.features.using.greedy(matches.for.training, features.for.predicting, create.knn.model.builder(k = 100), predict.type = "response")

# look at performance of the subset that makes most sense for us
most.sensful.features <- c(
 'attack.strength.home.team',
 'attack.strength.away.team',
 'win.ratio.home.team',
 'win.ratio.away.team',
 'win.ratio.home.team.playing.home',
 'win.ratio.away.team.playing.away'
)
most.sensful <- cross.validate.model(matches.for.training, create.knn.model.builder(k = 100), most.sensful.features, predict.type = "response")

best.feature.subset.knn.result <- results
best.feature.subset.knn.result[["most.sensful.CV.error"]] <- most.sensful
best.feature.subset.knn.result[["most.sensful.feature.subset"]] <- most.sensful.features

save(best.feature.subset.knn.result, file="./results/match_result/saved_Rdata/best_feature_subset_knn_result.Rdata")

