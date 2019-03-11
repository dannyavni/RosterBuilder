library(ggplot2)

player_plot <- function(roster) {
  if (nrow(roster) <= 1) {
    return(ggplot() + ggtitle("No Roster Was Computed"))
  }
  
  return(
    ggplot(roster, aes(x=PTS, y=plusminus,color=Pos)) +
      geom_point(alpha = 0.2, size = 12) +
      ggtitle("Selected Roster Points by +/- by Player") +
      xlab("Player Points") +
      ylab("Player +/-") +
      geom_text(label=roster$Player, show.legend = FALSE)
  )
  
}

roster_plot <- function(nhl, roster) {

  nhl <- nhl[nhl$Season=="2017", c("Tm","GP","PTS","plusminus")]

  if (nrow(roster) <= 1) {
    return(ggplot() + ggtitle("No Roster Was Computed"))
  }
  
  roster <- roster[,c("GP","PTS","plusminus")]
  roster$Tm <- 'Sockeyes'
  nhl <- rbind(nhl, roster)
  
  nhl$PTS_PG <- nhl$PTS/nhl$GP
  team_pts <- aggregate(PTS_PG ~ Tm  , data  = nhl, sum)
  team_plusminus <- aggregate(plusminus ~ Tm  , data  = nhl, mean)
  team_pts <- merge(team_pts, team_plusminus, by="Tm")
  
  team_pts$Playoffs <- "Failed"
  made_playoffs <- c('MTL','BOS','CBJ','TOR','CHI','MIN','SJS','AFM')
  made_2nd      <- c('NYR','WSH','STL','EDM')
  made_conf     <- c('OTT','ANA')
  made_finals   <- c('NSH')
  made_it       <- c('PIT')
  
  team_pts[team_pts$Tm %in%  made_playoffs,]$Playoffs <- "Made Playoffs"
  team_pts[team_pts$Tm %in%  made_2nd,]$Playoffs      <- "Made 2nd Round"
  team_pts[team_pts$Tm %in%  made_conf,]$Playoffs     <- "Made Conference"
  team_pts[team_pts$Tm %in%  made_finals,]$Playoffs   <- "Made Finals"
  team_pts[team_pts$Tm %in%  made_it,]$Playoffs       <- "Won!"
  
  team_pts[team_pts$Tm == "Sockeyes",]$Playoffs       <- "Sockeyes"
  
  return(
    ggplot(team_pts, aes(x=PTS_PG, y=plusminus, color=Playoffs)) +
      geom_point(alpha = 0.2, size = 12) +
      scale_colour_manual(values=c("darkgrey", "blue", "purple", "orange", "red", "darkgreen", "gold")) +
      ggtitle("NHL 2017 PTS per Game by +/- by Team") +
      xlab("Team Points Per Game") +
      ylab("Team +/-") +
      geom_text(label=team_pts$Tm, show.legend = FALSE)
  )
}