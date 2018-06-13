set.seed(123)

# Load matches with all relevant features for predicting match result (win ratio, attack strength...)
source('./loaders/load_matches_with_all_relevant_features_for_match_result.R')
source('./models/match_result/multinomial_regression.R')
source('./validation/compute_test_error.R')

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
  'win.ratio.away.team.playing.away'
)
features.for.predicting <- features.for.keeping[3:length(features.for.keeping)]
# select from dataframe only columns relevant for predicting
matches.for.training <- matches.merged.all.features[,features.for.keeping]
matches.for.training$target <- matches.for.training$result

cross.validated.best.features.for.predicting <- c(
  'target', # this needs to always be inside dataframe
  'attack.strength.home.team',
  'attack.strength.away.team',
  'win.ratio.home.team',
  'win.ratio.away.team',
  'win.ratio.home.team.playing.home',
  'win.ratio.away.team.playing.away'
)

# with potential small leakage
test.error.logistic.model <- compute.test.error(
  matches.for.training,
  make.multinomial.logistic.regression.model, 
  cross.validated.best.features.for.predicting,
  predict.type="class"
)

# without leakage
test.error.logistic.model.without.leakage <- compute.test.error.using.last.season(
  matches.for.training, 
  make.multinomial.logistic.regression.model, 
  cross.validated.best.features.for.predicting, 
  predict.type="class"
)
