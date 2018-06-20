# goals probability distribution - complete script

source('./loaders/load_matches_from_csv.R')

# libraries

library(ggplot2)
library(reshape)

# real distribution

matches = load.matches.from.csv()
matches = matches[which(matches$season == "2015/2016"),]
goals = c(matches$home_team_goal,matches$away_team_goal)
d = data.frame(table(goals))
d$real = d$Freq/sum(d$Freq)
d$Freq = NULL

# models distribution

d$glm.n = c(0.28,0.59,0.11,0.02,0.00,0.00,0.00,0.00,0.00,0.00,0.00) # obtained from ./glm.R L62
d$glm.p = c(0.26,0.63,0.08,0.02,0.00,0.00,0.00,0.00,0.00,0.00,0.00) # obtained from ./glm.R L62
d$rf    = c(0.03,0.60,0.31,0.05,0.01,0.00,0.00,0.00,0.00,0.00,0.00) # obtained from ./random_forests.R L65
d$knn   = c(0.00,0.68,0.28,0.04,0.00,0.00,0.00,0.00,0.00,0.00,0.00) # obtained from ./knn.R L68

# plot

d = melt(d,id='goals')
ggplot(d,aes(x=goals,y=value,colour=variable,group=variable)) + 
  geom_line(size=2) + 
  ylab("frequency") + 
  xlab("goals") +
  theme_bw() + 
  scale_colour_manual(name = "models", values=c("black","red","green","blue","orange"))
