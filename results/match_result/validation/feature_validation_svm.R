library(ggplot2)
library(stringr)

# First examine exhaustive strategy results
load('./results/match_result/saved_Rdata/best_feature_subset_svm_result.Rdata')

best.feature.subset.svm.result <- values(best.feature.subset.svm.result)
names.for.CV.error <- names(best.feature.subset.svm.result)[str_detect(names(best.feature.subset.svm.result), 'CV_error_')]
CV.errors.greedy <- best.feature.subset.svm.result[names.for.CV.error]

  
  
x.graph <- seq(1,length(CV.errors.greedy))
result.for.graph.greedy <- data.frame(x.graph, unlist(CV.errors.greedy))
colnames(result.for.graph.greedy)[2] <- 'CV.error'

x.point.best <- which(unlist(CV.errors.greedy) == best.feature.subset.svm.result$lowest.CV.error.seen)
x.point.best <- as.integer(x.point.best)
y.point.best <- unlist(CV.errors.greedy)[x.point.best]

ggplot(data = result.for.graph.greedy, aes(x = x.graph)) +
  theme_bw() + 
  geom_line(aes(y = CV.error)) +
  geom_point(aes(x = c(x.point.best), y = c(y.point.best), colour = c("red"))) + 
  scale_color_manual(labels = c("lowest CV error"), values = c("red")) +
  labs(x = "Iteration", 
       y = "10-fold cross validation error", 
       title = "Determining best feature subset for logistic using greedy strategy and CV",
       colour = "")

# Observe best subset of features
best.feature.subset.svm.result$best.subset.features

# observe difference between lowest CV error seen and what makes most sense
best.feature.subset.svm.result$lowest.CV.error.seen



