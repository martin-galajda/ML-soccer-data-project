# goals probability distribution - complete script

source('./loaders/load_matches_from_csv.R')

# libraries

library(ggplot2)
library(reshape)

# real distribution

matches = load.matches.from.csv()
goals = c(matches$home_team_goal,matches$away_team_goal)
d = data.frame(table(goals))
d$real = d$Freq/sum(d$Freq)
d$Freq = NULL

# models distribution

d$glm.n = c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00) # TODO
d$glm.p = c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00) # TODO
d$svr   = c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00) # TODO
d$rf    = c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00) # TODO
d$knn   = c(0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00) # TODO

# plot

d = melt(d,id='goals')
ggplot(d,aes(x=goals,y=value,colour=variable,group=variable)) + 
  geom_line(size=2) + 
  ylab("frequency") + 
  xlab("goals") + scale_x_discrete(limits = seq.int(0,10)) + 
  theme_bw() + 
  scale_colour_manual(name = "models", values=c("black","red","orange","green","blue","yellow"))
