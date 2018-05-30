source('./feature_extraction/attack_strength_ratio.R')
source('./loaders/load_matches_from_csv.R')

# This will precompute attack strength always with respect to all previous matches
# and save them in the data/ directory as "matches_with_attack_strength_from_prev_matches.csv"

filename <- './data/matches_with_attack_strength_from_prev_matches.csv'
log.period <- 100

# create file only if doesnt exist
if (!file.exists(filename)) {
  matches <- load.matches.from.csv()
  ordered.matches.by.date <- matches[order(matches$date), ]
  
  first.season <- '2008/2009'
  matches.without.first.season <- ordered.matches.by.date[ordered.matches.by.date$season != first.season,]
  
  relevant.features <- c("id", "result", "season", "home_team", "away_team", "league", "date", "home_team_goal", "away_team_goal")
  matches.without.first.season <- matches.without.first.season[, relevant.features]
  
  
  enhanced.matches <- data.frame()
  number.of.matches <- dim(matches.without.first.season)[1]
  for (match.number in 1:number.of.matches) {
    match <- matches.without.first.season[match.number, ]
    
    attack.strengths <- compute.attack.strengths(ordered.matches.by.date, match)
    
    enhanced.match <- cbind(match, attack.strengths)
    enhanced.matches <- rbind(enhanced.matches, enhanced.match)
    
    if (match.number %% log.period == 0) {
      print(paste("Processed ", match.number, " out of ", number.of.matches, sep = ""))
    }
  }
  
  matches.with.attack.strength <- enhanced.matches[!is.na(enhanced.matches$attack.strength.home.team), ]
  matches.with.attack.strength <- matches.with.attack.strength[!is.na(matches.with.attack.strength$attack.strength.away.team), ]
  
  # write to csv file
  write.csv(matches.with.attack.strength, filename)
}
