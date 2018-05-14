library('RSQLite')
library('XML')

con = dbConnect(drv=dbDriver("SQLite"),dbname="data/database.sqlite")

# load all databases

countries = dbGetQuery(con,"select * from Country")
leagues   = dbGetQuery(con,"select * from League")
matches   = dbGetQuery(con,"select * from Match")
players   = dbGetQuery(con,"select * from Player")
p_attrs   = dbGetQuery(con,"select * from Player_Attributes")
teams     = dbGetQuery(con,"select * from Team")
t_attrs   = dbGetQuery(con,"select * from Team_Attributes")

# create db_matches dataset

db_matches = matches
db_matches$id = NULL

# add country

db_matches = merge(db_matches,countries,by.x=c("country_id"),by.y=c("id"))
db_matches$country = db_matches$name
db_matches[,c("country_id","name")] = NULL

# add league

db_matches = merge(db_matches,leagues,by.x=c("league_id"),by.y=c("id"))
db_matches$league = db_matches$name
db_matches[,c("league_id","country_id","name")] = NULL

# add home team

db_matches = merge(db_matches,teams,by.x=c("home_team_api_id"),by.y=c("team_api_id"))
db_matches$home_team = db_matches$team_long_name
db_matches[,c("id","team_fifa_api_id","team_long_name","team_short_name")] = NULL

# add away team

db_matches = merge(db_matches,teams,by.x=c("away_team_api_id"),by.y=c("team_api_id"))
db_matches$away_team = db_matches$team_long_name
db_matches[,c("id","team_fifa_api_id","team_long_name","team_short_name")] = NULL

# add "complex" variables

countVars = function(var,home,away) {
  h_var = rep(NA, length(var))
  a_var = rep(NA, length(var))
  for (i in 1:length(var)) {
    if (!is.na(var[i]) && nchar(var[i]) > 20) {
      counters = table(xmlToDataFrame(getNodeSet(xmlParse(var[i]),"//value"))$team)
      h_var[i] = ifelse(toString(home[i]) %in% names(counters), counters[toString(home[i])], 0)
      a_var[i] = ifelse(toString(away[i]) %in% names(counters), counters[toString(away[i])], 0)
    }
  }
  return (c(h_var,a_var))
}

h_ts = db_matches$home_team_api_id
a_ts = db_matches$away_team_api_id
db_matches[,c("home_shoton","away_shoton")]         = countVars(db_matches$shoton,h_ts,a_ts)
db_matches[,c("home_shotoff","away_shotoff")]       = countVars(db_matches$shotoff,h_ts,a_ts)
db_matches[,c("home_foulcommit","away_foulcommit")] = countVars(db_matches$foulcommit,h_ts,a_ts)
db_matches[,c("home_card","away_card")]             = countVars(db_matches$card,h_ts,a_ts)
db_matches[,c("home_cross","away_cross")]           = countVars(db_matches$cross,h_ts,a_ts)
db_matches[,c("home_corner","away_corner")]         = countVars(db_matches$corner,h_ts,a_ts)

# TODO; add teams possesion
# TODO: add players info (players database + granular shoton, fouls...)
# TODO: add players position (understand variables home_player_X...)
# TODO: add players & teams FIFA attributes (p_attrs/t_attrs)

db_matches$result <- rep(NA, dim(matches)[1])
db_matches$result[db_matches$home_team_goal > db_matches$away_team_goal] = "Home win"
db_matches$result[db_matches$home_team_goal < db_matches$away_team_goal] = "Away win"
db_matches$result[db_matches$home_team_goal == db_matches$away_team_goal] = "Tie"
db_matches$result <- as.factor(db_matches$result)

db_matches = db_matches[,c("season","stage","date","home_team_goal","away_team_goal","country",
                           "league","home_team","away_team","B365H","B365D","B365A","home_shoton",
                           "away_shoton","home_shotoff","away_shotoff","home_foulcommit",
                           "away_foulcommit","home_card","away_card","home_cross","away_cross",
                           "home_corner","away_corner", "result")]

write.csv(db_matches,"data/db_matches.csv")
