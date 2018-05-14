# Compute ratios of matches won / lost / tied for given team.
# It expects parameters "start.date" and "end.date" to contain dates for which ratios should be computed.
# It expects name of the team in paramter called "team".
# Optionally, it is possible to fcompute ratios against specific team by setting against.team parameter.
# Returns results as data frame.
compute_ratios <- function(matches_csv, start.date, end.date, team, against.team = NULL) {
  filtered.matches.home <- matches_csv[as.character(matches_csv$home_team) == team 
                                       & as.Date(matches_csv$date) >= start.date 
                                       & as.Date(matches_csv$date) <= end.date,
                                       ]
  filtered.matches.away <- matches_csv[as.character(matches_csv$away_team) == team 
                                       & as.Date(matches_csv$date) >= start.date 
                                       & as.Date(matches_csv$date) <= end.date,
                                       ]
  number.of.all.matches <- dim(filtered.matches.home)[1] + dim(filtered.matches.away)[1]
  
  if (is.character(against.team)) {
    filtered.matches.home <- filtered.matches.home[as.character(filtered.matches.home$away_team) == against.team,]
    filtered.matches.away <- filtered.matches.away[as.character(filtered.matches.away$home_team) == against.team,]
  }
  
  matches.won.home <- filtered.matches.home[filtered.matches.home$result == "Home win", ]
  matches.won.away <- filtered.matches.away[filtered.matches.away$result == "Away win", ]
  matches.tie.home <- filtered.matches.home[filtered.matches.home$result == "Tie", ]
  matches.tie.away <- filtered.matches.away[filtered.matches.away$result == "Tie", ]
  
  number.of.matches.won <- dim(matches.won.home)[1] + dim(matches.won.away)[1]
  
  win.ratio <- number.of.matches.won / number.of.all.matches
  win.ratio.home <- dim(matches.won.home)[1] / dim(filtered.matches.home)[1]
  win.ratio.away <- dim(matches.won.away)[1] / dim(filtered.matches.away)[1]
  tie.ratio.home <- dim(matches.tie.home)[1] / dim(filtered.matches.home)[1]
  tie.ratio.away <- dim(matches.tie.away)[1] / dim(filtered.matches.away)[1]
  
  result = data.frame(win.ratio, win.ratio.home, win.ratio.away, tie.ratio.home, tie.ratio.away)
  
  return(result)
}


# Example usage:
# ratios = compute_ratios(matches_csv, start.date = as.Date("1990-10-15"), end.date = as.Date("2018-01-14"), team = "PEC Zwolle", against.team = "FC Groningen")
