library(ggplot2)
library(stringr)

# First examine exhaustive strategy results
load('./results/match_result/saved_Rdata/best_feature_subset_logistic_result_exhaustive.Rdata')
names.for.CV.error <- names(best.feature.subset.logistic.exhaustive)[str_detect(names(best.feature.subset.logistic.exhaustive), 'CV.error_')]
CV.errors <- best.feature.subset.logistic.exhaustive[names.for.CV.error]

x.graph <- seq(1,length(CV.errors))
result.for.graph <- data.frame(x.graph, unlist(CV.errors))
colnames(result.for.graph)[2] <- 'CV.error'

lowest.error.x <- which(CV.errors == best.feature.subset.logistic.exhaustive$lowest.CV.error.seen)
lowest.error.y <- unlist(CV.errors)[lowest.error.x]

# What makes most sense for us
best.feature.subset.logistic.exhaustive$CV.subset_features_6_3

# Feature combination which logically makes most sense for us
most.common.sense.appropriate.error <- best.feature.subset.logistic.exhaustive$CV.error_6_3
most.common.sense.appropriate.x <- which(CV.errors == most.common.sense.appropriate.error)
most.common.sense.appropriate.y <- unlist(CV.errors)[most.common.sense.appropriate.x]

ggplot(data = result.for.graph, aes(x = x.graph)) +
  theme_bw() + 
  geom_line(aes(y = CV.error)) +
  geom_point(aes(x = c(lowest.error.x), y = c(lowest.error.y), colour = c("red"))) + 

  geom_point(aes(x = c(most.common.sense.appropriate.x), y = c(most.common.sense.appropriate.y), colour = c("green"))) + 
  scale_color_manual(labels = c("lowest CV error", "most common sense appropriate error"), values = c("red", "green")) +
  labs(x = "Index of combination used", 
       y = "10-fold cross validation error", 
       title = "Determining best feature subset for logistic using 10x10-fold CV",
       colour = "")

# Now examine greedy approach result
load('./results/match_result/saved_Rdata/best_feature_subset_logistic_result.Rdata')
best.feature.subset.logistic.result <- values(best.feature.subset.logistic.result)
names.for.CV.error.2 <- names(best.feature.subset.logistic.result)[str_detect(names(best.feature.subset.logistic.result), 'CV_error_')]
CV.errors.greedy <- best.feature.subset.logistic.result[names.for.CV.error.2]

x.graph <- seq(1,length(CV.errors.greedy))
result.for.graph.greedy <- data.frame(x.graph, unlist(CV.errors.greedy))
colnames(result.for.graph.greedy)[2] <- 'CV.error'

ggplot(data = result.for.graph.greedy, aes(x = x.graph)) +
  theme_bw() + 
  geom_line(aes(y = CV.error)) +
  labs(x = "Iteration", 
       y = "10-fold cross validation error", 
       title = "Determining best feature subset for logistic using greedy strategy and CV",
       colour = "")

# Observe best subset of features
best.feature.subset.logistic.exhaustive$best.subset.features
best.feature.subset.logistic.result$best.subset.features

# observe difference between lowest CV error seen and what makes most sense
best.feature.subset.logistic.exhaustive$lowest.CV.error.seen - best.feature.subset.logistic.exhaustive$CV.error_6_3
# Difference is very negligible, so we use the feature subset which makes most sense



