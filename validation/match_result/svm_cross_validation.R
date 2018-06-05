source('./loaders/load_matches_with_all_relevant_features_for_match_result.R')
source('./models/match_result/svm.R')
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

# Determine best kernel and best C
possible.kernels <- c("rbfdot", "polydot", "vanilladot")
possible.C.values <- seq(1, 201, 50)

result <- list()
result = vector(mode="list", length=length(possible.kernels) * length(possible.C))

for (possible.kernel in possible.kernels) {
  for (possible.C in possible.C.values) {
    name <- paste(possible.kernel, "_C_", possible.C, sep = "")
    print(name)
    
    CV.error <- cross.validate.model(matches.for.training, build.model = create.svm.model.builder(kernel=possible.kernel, C = possible.C), cross.validated.best.features.for.predicting)
    result[[name]] = CV.error
  }
}

save(result, file = "cv_results_match_result_svm.Rdata")


