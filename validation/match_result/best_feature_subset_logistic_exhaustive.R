source('./loaders/load_matches_with_all_relevant_features_for_match_result.R')
source('./models/match_result/multinomial_regression.R')
source('./validation/determine_best_feature_subset.R')

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
# This is very exhaustive search, using all possible combinations of features and using 10x10 CV
# Takes A LOT OF TIME to compute
results <- determine.best.subset.of.features(matches.for.training, features.for.predicting, make.multinomial.logistic.regression.model, predict.type = "class", repeat.k.fold = 10)

best.feature.subset.logistic.exhaustive <- results

best.feature.subset.logistic.exhaustive <- values(best.feature.subset.logistic.exhaustive)

save(best.feature.subset.logistic.exhaustive, file="./results/match_result/saved_Rdata/best_feature_subset_logistic_result_exhaustive.Rdata")


