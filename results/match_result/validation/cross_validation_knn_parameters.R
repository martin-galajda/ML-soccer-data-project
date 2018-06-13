library(ggplot2)

load('./results/match_result/saved_Rdata/CV_error_knn_overal_best_k.Rdata')

x.axis <- c(seq(1,100,5), seq(101,401,5))

results <- data.frame(x.axis, CV.error.knn.overal)

min.idx <- which(CV.error.knn.overal == min(CV.error.knn.overal))

ggplot(data = results, aes(x = x.axis)) + 
  theme_bw() + 
  geom_line(aes(y = CV.error.knn.overal)) +
  geom_point(aes(x = x.axis[min.idx], y = CV.error.knn.overal[min.idx], colour = "red" )) +
  scale_color_manual(labels = c(paste("minimum value (k = ", x.axis[min.idx], ")", sep = "")), values = c("red")) +
  labs(x = "k used for KNN", 
       y = "10-fold cross validation error", 
       title = "Determining best k parameter for knn using 10-fold CV",
       colour = "")
