library(lpSolve)

make_roster <- function(roster, total_pts, maxAvgAge, probThreshold) {
  i = length(unique(roster$Player))
  
  # constraints
  cons = rbind(
    rep(1,i), # 30 man constraint 
    sapply(roster$Source_Team, function(x) if (x == "Anaheim Ducks") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Arizona Coyotes") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Boston Bruins") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Buffalo Sabres") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Calgary Flames") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Carolina Hurricanes") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Chicago Blackhawks") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Colorado Avalanche") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Columbus Blue Jackets") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Dallas Stars") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Detroit Red Wings") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Edmonton Oilers") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Florida Panthers") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Los Angeles Kings") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Minnesota Wild") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Montreal Canadiens") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Nashville Predators") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "New Jersey Devils") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "New York Islanders") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "New York Rangers") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Ottawa Senators") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Philadelphia Flyers") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Pittsburgh Penguins") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "San Jose Sharks") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "St. Louis Blues") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Tampa Bay Lightning") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Toronto Maple Leafs") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Vancouver Canucks") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Washington Capitals") x=1 else x=0),
    sapply(roster$Source_Team, function(x) if (x == "Winnipeg Jets") x=1 else x=0),
    sapply(roster$Pos, function(x) if (x == "C") x=1 else x=0),
    sapply(roster$Pos, function(x) if (x == "D") x=1 else x=0),
    sapply(roster$Pos, function(x) if (x == "LW") x=1 else x=0),
    sapply(roster$Pos, function(x) if (x == "RW") x=1 else x=0),
    sapply(roster$Pos, function(x) if (x == "G") x=1 else x=0),
    as.vector(roster$Age),
    as.vector(roster$CAP),
    as.vector(roster$CAP),
    as.vector(roster$P_Upside),
    as.vector(roster$PTS),
    sapply(roster$Status, function(x) if (x == "Include") x=1 else x=0),
    sapply(roster$Status, function(x) if (x == "Exclude") x=1 else x=0)
  )
  
  include <- nrow(roster[roster$Status == "Include",])

  # model
  f.obj = as.vector(roster$plusminus)
  f.dir = c("=", rep("=",30), rep("=",5),    "<=",          ">=",     "<=",      ">=",   ">=",       "=",       "=")
  f.rhs = c(30,  rep(1,30),   6, 9, 6, 6, 3, maxAvgAge*30,  40000000, 175000000, probThreshold*30,   total_pts, include, 0)
  
  model = lp("max", f.obj, cons, f.dir, f.rhs, all.bin=T,compute.sens=1)

  roster$Selected <- model$solution

  return(roster[roster$Selected ==1,])
}