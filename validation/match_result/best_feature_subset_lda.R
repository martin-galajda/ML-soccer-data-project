source('./loaders/load_matches_with_all_relevant_features_for_match_result.R')
source('./models/match_result/lda.R')
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
results <- determine.best.subset.of.features.using.greedy(matches.for.training, features.for.predicting, make.lda.model, predict.type = "response", repeat.k.fold = 3)

best.feature.subset.lda.result <- results

save(best.feature.subset.lda.result, file="./results/match_result/saved_Rdata/best_feature_subset_lda_result.Rdata")


