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


compute_win_ratio_for_match <- function(matches, match, num.of.prev.matches.for.ratio = 3) {
  # we expect matches to be ordered by date
  # ordered.matches <- matches[order(matches$date), ]
  
  filtered.matches.home <- ordered.matches[as.character(ordered.matches$home_team) == match$home_team 
                                       & as.Date(ordered.matches$date) < match$date,
                                       ]
  
  filtered.matches.away <- ordered.matches[as.character(ordered.matches$away_team) == match$away_team 
                                       & as.Date(ordered.matches$date) < match$date,
                                       ]
  
  # We want to take into account only last "num.of.prev.matches.for.ratio" matches
  filtered.matches.home <- tail(filtered.matches.home, num.of.prev.matches.for.ratio)
  filtered.matches.away <- tail(filtered.matches.away, num.of.prev.matches.for.ratio)
  
  number.of.all.matches <- dim(filtered.matches.home)[1] + dim(filtered.matches.away)[1]
  
  matches.won.home <- filtered.matches.home[filtered.matches.home$result == "Home win", ]
  matches.won.away <- filtered.matches.away[filtered.matches.away$result == "Away win", ]
  
  matches.lost.home <- filtered.matches.home[filtered.matches.home$result == "Away win", ]
  matches.lost.away <- filtered.matches.away[filtered.matches.away$result == "Home win", ]
  
  matches.tie.home <- filtered.matches.home[filtered.matches.home$result == "Tie", ]
  matches.tie.away <- filtered.matches.away[filtered.matches.away$result == "Tie", ]
  
  number.of.matches.won <- dim(matches.won.home)[1] + dim(matches.won.away)[1]
  number.of.matches.tied <- dim(matches.tie.home)[1] + dim(matches.tie.away)[1]
    
  win.ratio <- number.of.matches.won / number.of.all.matches
  
  win.ratio.with.ties <- (number.of.matches.won + number.of.matches.tied) / number.of.all.matches
  
  win.ratio.home <- dim(matches.won.home)[1] / dim(filtered.matches.home)[1]
  win.ratio.away <- dim(matches.won.away)[1] / dim(filtered.matches.away)[1]
  tie.ratio.home <- dim(matches.tie.home)[1] / dim(filtered.matches.home)[1]
  tie.ratio.away <- dim(matches.tie.away)[1] / dim(filtered.matches.away)[1]
  loss.ratio.home <- dim(matches.lost.home)[1] / dim(filtered.matches.home)[1]
  loss.ratio.away <- dim(matches.lost.away)[1] / dim(filtered.matches.away)[1]
  
  win.ratio.home.with.ties = (dim(matches.won.home)[1] + (dim(matches.tie.home)[1] * 0.5)) / dim(filtered.matches.home)[1]
  win.ratio.away.with.ties = (dim(matches.won.away)[1] + (dim(matches.tie.away)[1] * 0.5)) / dim(filtered.matches.away)[1]
  
  result = data.frame(
    win.ratio,
    win.ratio.home,
    win.ratio.away,
    tie.ratio.home,
    tie.ratio.away,
    loss.ratio.home,
    loss.ratio.away,
    win.ratio.home.with.ties,
    win.ratio.away.with.ties
  )
  
  return(result)
}

# Example usage:
# ratios = compute_ratios(matches_csv, start.date = as.Date("1990-10-15"), end.date = as.Date("2018-01-14"), team = "PEC Zwolle", against.team = "FC Groningen")
