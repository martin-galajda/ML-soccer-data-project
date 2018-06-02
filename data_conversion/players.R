library('RSQLite')
library('XML')

con = dbConnect(drv=dbDriver("SQLite"),dbname="data/database.sqlite")

# create db_players
db_players = dbGetQuery(con,"select * from Player")
db_players$id = NULL

# add FIFA attributes
p_attrs = dbGetQuery(con,"select * from Player_Attributes")
db_players = merge(db_players,p_attrs)

matches = dbGetQuery(con,"select * from Match")

# add # of games
x = data.frame()
for (i in 1:11) {
  x = rbind(x,as.data.frame(table(matches[,paste0("home_player_",i)])))
  x = rbind(x,as.data.frame(table(matches[,paste0("away_player_",i)])))
}
db_players = merge(db_players,aggregate(Freq ~ Var1, x, sum),by.x="player_api_id",by.y="Var1")
db_players$games = db_players$Freq
db_players$Freq = NULL

# add # of goals
numGoals = function(db,var) {
  db = data.frame(player_api_id=db,goals=0)
  for (i in 1:length(var)) {
    if (is.na(var[i]) || nchar(var[i]) < 20) next
    x = xpathSApply(xmlParse(var[i]),"//player1",xmlValue)
    if (length(x) <= 0) next
    for (j in 1:length(x))
      db[db$player_api_id == x[j],]$goals = db[db$player_api_id == x[j],]$goals + 1
  }
  return (db)
}
goals = numGoals(unique(db_players$player_api_id),matches$goal)
db_players = merge(db_players,goals,by="player_api_id")

write.csv(db_players,"data/db_players.csv")
dbDisconnect(con)
