library('RSQLite')

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
db_matches[,c("id","home_team_api_id","team_fifa_api_id","team_long_name","team_short_name")] = NULL

# add away team
db_matches = merge(db_matches,teams,by.x=c("away_team_api_id"),by.y=c("team_api_id"))
db_matches$away_team = db_matches$team_long_name
db_matches[,c("id","away_team_api_id","team_fifa_api_id","team_long_name","team_short_name")] = NULL

# TODO: add players basic info (players database)
# TODO: add players position (understand variables home_player_X...)
# TODO: add player goals, shots... (understand string variables goal, shoton...)
# TODO: add players & teams FIFA attributes (p_attrs/t_attrs)

# USE ONLY CURRENTLY HELPFUL VARIABLES

db_matches = db_matches[,c("season","stage","date","home_team_goal","away_team_goal","country",
                           "league","home_team","away_team","B365H","B365D","B365A")]

write.csv(db_matches,"data/db_matches.csv")
