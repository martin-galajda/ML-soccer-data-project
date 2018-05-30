# This is main file for running different models for predicting match result
# It uses functions defined in this directory to assess different models

# TODO: Implement knn model
# source('./models/match_result/knn.R')
# TODO: Implement tree-based model?
# TODO: Implement SVM model (extended for 3 classes)?

# Load matches with all relevant features for predicting match result (win ratio, attack strength...)
source('./loaders/load_matches_with_all_relevant_features_for_match_result.R')
matches.merged.all.features <- load.matches.with.all.features.for.match.result()

# Select features for predicting
features.for.predicting <- c(
  'result', 
  'attack.strength.home.team',
  'win.ratio',
  'win.ratio.home.with.ties',
  'win.ratio.away.with.ties',
  'tie.ratio.home',
  'tie.ratio.away'
)
# select from dataframe only columns relevant for predicting
matches.for.training <- matches.merged.all.features[,features.for.predicting]

# TODO: Run cross-validation to select right subset of features for predicting and the best model
# TODO: Make ROC curve to assess best model
source('./models/match_result/lda.R')
lda.model <- make.lda.model(matches.for.training)

source('./models/match_result/qda.R')
qda.model <- make.qda.model(matches.for.training)

source('./models/match_result/multinomial_regression.R')
multinomial.model <- make.multinomial.logistic.regression.model(matches.for.training)

