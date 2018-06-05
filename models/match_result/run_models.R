# This is main file for running different models for predicting match result
# It uses functions defined in this directory to assess different models

# TODO: Implement knn model
# source('./models/match_result/knn.R')
# TODO: Implement tree-based model?
# TODO: Implement SVM model (extended for 3 classes)?

# Load matches with all relevant features for predicting match result (win ratio, attack strength...)
source('./loaders/load_matches_with_all_relevant_features_for_match_result.R')
source('./models/match_result/lda.R')
source('./models/match_result/qda.R')
source('./models/match_result/multinomial_regression.R')
source('./models/match_result/knn.R')
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

CV.error.lda <- cross.validate.model(matches.for.training, build.model = make.lda.model, cross.validated.best.features.for.predicting)
CV.error.lda

CV.error.qda <- cross.validate.model(matches.for.training, build.model = make.qda.model, cross.validated.best.features.for.predicting)
CV.error.qda

CV.error.multinomial <- cross.validate.model(matches.for.training, build.model = make.multinomial.logistic.regression.model, cross.validated.best.features.for.predicting)
CV.error.multinomial

source('./models/match_result/svm.R')
source('./validation/cross_validate.R')
CV.error.svm.C.1 <- cross.validate.model(matches.for.training, build.model = create.svm.model.builder(kernel="vanilladot", C = 1), cross.validated.best.features.for.predicting)
CV.error.svm.C.1

CV.error.svm.C.100 <- cross.validate.model(matches.for.training, build.model = create.svm.model.builder(kernel="vanilladot", C = 100), cross.validated.best.features.for.predicting)
CV.error.svm.C.100


svp <- ksvm(as.matrix(matches.for.training[, features.for.predicting]),matches.for.training$target,type="C-svc",kernel="vanilladot",C=100,scaled=c())
preds <- predict(svp, newdata=matches.for.training[, features.for.predicting], type="response")


source('./models/match_result/knn.R')
k.length <- 100
CV.error.knn.from.355.to.400 <- c()
for (i in seq(355,400, 5)) {
  CV.error.knn.from.355.to.400 = c(CV.error.knn.from.355.to.400, cross.validate.model(
    matches.for.training,
    build.model = create.knn.model.builder(k = i),
    cross.validated.best.features.for.predicting,
    predict.type="class"
  ))
}




source('./validation/determine_best_feature_subset.R')
# used for determining best features (takes a long time compute, tries all combinations)
# res.best.features.lda <- determine.best.subset.of.features(matches.for.training, features.for.predicting, make.lda.model)
# best.subset.of.features.lda <- values(res.best.features.lda)[["best.subset.features"]]
# res.best.features.logistic <- determine.best.subset.of.features(matches.for.training, features.for.predicting, make.multinomial.logistic.regression.model, predict.type = "class")
# best.subset.of.features.logistic <- values(res.best.features.logistic)[["best.subset.features"]]

# TODO: Make ROC curve to assess best model

source('./validation/compute_test_error.R')
test.error.lda.model <- compute.test.error(matches.for.training, make.lda.model, cross.validated.best.features.for.predicting)
test.error.lda.model.2 <- compute.test.error.using.last.season(matches.for.training, make.lda.model, cross.validated.best.features.for.predicting)

test.error.qda.model <- compute.test.error(matches.for.training, make.qda.model, cross.validated.best.features.for.predicting)
test.error.qda.model.2 <- compute.test.error.using.last.season(matches.for.training, make.qda.model, cross.validated.best.features.for.predicting)

test.error.logistic.model <- compute.test.error(matches.for.training, make.multinomial.logistic.regression.model, cross.validated.best.features.for.predicting, predict.type="class")
test.error.logistic.model.2 <- compute.test.error.using.last.season(matches.for.training, make.multinomial.logistic.regression.model, cross.validated.best.features.for.predicting, predict.type="class")

k = 150
test.error.knn.model <- compute.test.error(matches.for.training, create.knn.model.builder(k = k), cross.validated.best.features.for.predicting, predict.type="class")
test.error.knn.model.2 <- compute.test.error.using.last.season(matches.for.training, create.knn.model.builder(k = k), cross.validated.best.features.for.predicting, predict.type="class")
test.error.knn.model
test.error.knn.model.2

lda.model <- make.lda.model(matches.for.training, cross.validated.best.features.for.predicting)
multinomial.model <- make.multinomial.logistic.regression.model(matches.for.training, cross.validated.best.features.for.predicting)