library(ggplot2)
# This is main file for running different models for predicting match result
# It uses functions defined in this directory to assess different models

set.seed(123)

# Load matches with all relevant features for predicting match result (win ratio, attack strength...)
source('./loaders/load_matches_with_all_relevant_features_for_match_result.R')
source('./models/match_result/lda.R')
source('./models/match_result/qda.R')
source('./models/match_result/multinomial_regression.R')
source('./models/match_result/knn.R')
source('./models/match_result/svm.R')
source('./validation/compute_test_error.R')
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
cross.validated.best.features.for.svm <- c(
  'target', # this needs to always be inside dataframe
  'attack.strength.home.team',
  'attack.strength.away.team',
  'win.ratio.home.team.playing.home',
  'win.ratio.away.team.playing.away'
)

cv.error.lda.model <- cross.validate.model(matches.for.training, make.lda.model, cross.validated.best.features.for.predicting, predict.type = "response")

cv.error.qda.model <- cross.validate.model(matches.for.training, make.qda.model, cross.validated.best.features.for.predicting, predict.type = "response")

cv.error.logistic.model <- cross.validate.model(matches.for.training, make.multinomial.logistic.regression.model, cross.validated.best.features.for.predicting, predict.type = "class")

# cross-validated best k
k = 296
cv.error.knn.model <- cross.validate.model(matches.for.training, create.knn.model.builder(k = k), cross.validated.best.features.for.predicting, predict.type = "class")

cv.error.svm.model <- cross.validate.model(matches.for.training, create.svm.model.builder(kernel = "rbfdot", C = 1), cross.validated.best.features.for.svm, predict.type = "response")

CV.errors.models <- c('lda', 'qda', 'multinomial', 'knn', 'svm')
CV.errors.values <- c(
  cv.error.lda.model,
  cv.error.qda.model,
  cv.error.logistic.model,
  cv.error.knn.model,
  cv.error.svm.model
)

df <- data.frame(CV.errors.models, CV.errors.values)
df$CV.errors.values <- round(df$CV.errors.values, digits = 4)

ggplot(data=df, aes(x=CV.errors.models, y=CV.errors.values, fill=CV.errors.models)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=CV.errors.values), vjust=-1) +
  coord_cartesian(ylim = c(0.43, 0.452)) + 
  labs(x = "", 
       title = "10-fold cross validation error",
       fill = "Model") +
  theme_minimal() +
  theme(plot.title = element_text(size=22, hjust = 0.5),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size=16),
        axis.title.x = element_text(size=16, vjust = -5),
        axis.title.y= element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill=FALSE)

# here we can see that the best model is multinomial
