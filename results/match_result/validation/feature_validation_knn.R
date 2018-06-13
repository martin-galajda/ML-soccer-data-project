library(ggplot2)
library(stringr)

# First examine exhaustive strategy results
load('./results/match_result/saved_Rdata/best_feature_subset_knn_result.Rdata')

best.feature.subset.knn.result <- values(best.feature.subset.knn.result)
names.for.CV.error <- names(best.feature.subset.knn.result)[str_detect(names(best.feature.subset.knn.result), 'CV_error_')]
CV.errors.greedy <- best.feature.subset.knn.result[names.for.CV.error]

x.graph <- seq(1,length(CV.errors.greedy))
x.most.sensful <- length(x.graph) + 1
x.graph <- c(x.graph, x.most.sensful)

y.most.sensful <- best.feature.subset.knn.result$most.sensful.CV.error
CV.errors.greedy.unlisted <- unlist(CV.errors.greedy)
CV.errors.greedy.unlisted[["most.sensful"]] <- y.most.sensful

result.for.graph.greedy <- data.frame(x.graph, CV.errors.greedy.unlisted)
colnames(result.for.graph.greedy)[2] <- 'CV.error'

x.point.best <- which(CV.errors.greedy.unlisted == best.feature.subset.knn.result$lowest.CV.error.seen)
x.point.best <- as.integer(x.point.best)
y.point.best <- CV.errors.greedy.unlisted[x.point.best]

most.sensful.data <- data.frame(x.most.sensful, y.most.sensful)
best.data <- data.frame(x.point.best, y.point.best)

ggplot(data = result.for.graph.greedy, aes(x = x.graph)) +
  theme_bw() + 
  geom_line(aes(y = CV.error)) +
  geom_point(aes(x = c(x.most.sensful), y = c(y.most.sensful), colour = c("blue")), most.sensful.data) + 
  geom_point(aes(x = c(x.point.best), y = c(y.point.best), colour = c("red")), best.data) + 

  scale_color_manual(labels = c("most sensful CV error", "lowest CV error"), values = c( "green", "red")) +
  labs(x = "Iteration", 
       y = "10-fold cross validation error for feature selection for knn", 
       title = "Determining best feature subset for logistic using greedy strategy and CV",
       colour = "")

# Observe best subset of features
best.feature.subset.knn.result$best.subset.features

# observe difference between lowest CV error seen and what makes most sense
best.feature.subset.knn.result$lowest.CV.error.seen



