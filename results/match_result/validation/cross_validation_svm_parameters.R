# Let's observe cross-validation results for SVM using different kernels
# and different C and pick the best one
# The cross validation method used = 10-fold CV
library(ggplot2)

# Be sure to execute from the root dir of the project
load('./results/match_result/saved_Rdata/cv_results_match_result_svm.Rdata')

unlisted.CV.error.result <- unlist(CV.error.svm.models)
best.svm.model <- which(unlisted.CV.error.result == min(unlisted.CV.error.result))

print(paste('Best SVM model is ', names(unlisted.CV.error.result)[best.svm.model], sep =""))

x.graph <- seq(1,length(unlisted.CV.error.result))
result.for.graph <- data.frame(x.graph, unlisted.CV.error.result)

lowest.error.x <- best.svm.model
lowest.error.y <- unlisted.CV.error.result[lowest.error.x]

ggplot(data = result.for.graph, aes(x = x.graph)) +
  theme_bw() + 
  geom_line(aes(y = unlisted.CV.error.result)) +
  geom_point(aes(x = c(lowest.error.x), y = c(lowest.error.y), colour = c("red"))) + 
  scale_color_manual(labels = c("lowest CV error - rbf kernel with C = 1"), values = c("red")) +

  labs(x = "Iteration (kernel, C combination)", 
       y = "10-fold cross validation error", 
       title = "Determining best parameters for SVM using 10-fold CV",
       colour = "")
