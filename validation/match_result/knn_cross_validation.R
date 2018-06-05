source('./loaders/load_matches_with_all_relevant_features_for_match_result.R')
source('./models/match_result/knn.R')
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

cross.validated.best.features.for.predicting <- c(
  'target', # this needs to always be inside dataframe
  'attack.strength.home.team',
  'attack.strength.away.team',
  'win.ratio.home.team',
  'win.ratio.away.team',
  'win.ratio.home.team.playing.home',
  'win.ratio.away.team.playing.away'
)

CV.error.knn.from.1.to.400 <- c()
for (i in seq(1,401, 5)) {
  CV.error.knn.from.1.to.400 = c(CV.error.knn.from.1.to.400, cross.validate.model(
    matches.for.training,
    build.model = create.knn.model.builder(k = i),
    cross.validated.best.features.for.predicting,
    predict.type="class"
  ))
}

save(CV.error.knn.from.1.to.400, file = "cv_results_match_results_knn.svm")
