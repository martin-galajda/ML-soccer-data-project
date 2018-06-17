# analyze players - complete script

# libraries

library(corrplot)
library(cclust)
library(ggplot2)

# data loading

data = read.csv("data/db_players.csv")

# variables creation

data$age = as.numeric(difftime(as.Date(data$date),as.Date(data$birthday),units="weeks"))/52.25
data$lefty = as.numeric(data$preferred_foot == "left")

# anecdotal facts

tallest = data[which(data$height == max(data$height)),c("player_name","height")][1,]
most_goals = data[which(data$goals == max(data$goals)),c("player_name","goals")][1,]

# correlations

features = sample(c("age","height","weight","overall_rating","crossing","finishing","goals",
                    "heading_accuracy","volleys","dribbling","free_kick_accuracy","games",
                    "long_passing","ball_control","acceleration","agility","reactions",
                    "balance","shot_power","jumping","stamina","strength","long_shots","aggression",
                     "interceptions","positioning","vision","penalties","marking","standing_tackle"))
data.clean = na.omit(data[,features])
corrplot(cor(data.clean[,features]), method = "circle")

# clustering

features2 = c("age","height","weight","overall_rating","games","goals")
matrix.clean = as.matrix(scale(data.clean[,features2]))
N = 10
K = 2:10
bestCH = 0
bestCHs = rep(0,9)
bestK = 0
bestC = 0
for (k in K) {
  for (i in 1:N) {
    C = cclust(matrix.clean,k,iter.max=1000,method="kmeans",dist="euclidean")
    CH = as.numeric(clustIndex(C,matrix.clean,index="calinski"))
    if (CH > bestCH) {
      bestCH = CH
      bestK = k
      bestC = C
    }
    if (CH > bestCHs[k-1]) {
      bestCHs[k-1] = CH
    }
  }
}
qplot(K,bestCHs,xlab="K", ylab="Calinski-Harabasz") + geom_line(color="red") + theme_bw()
centers = bestC$centers
