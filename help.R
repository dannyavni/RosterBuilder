help_text <- as.data.frame(
  rbind(
  c("Compute","Crunch the roster and update table and plots. Goal is to maximize team's total +/-"),
  c("Export","Download roster as a CSV file"),
  c("Save","Save settings. Not implemented yet"),
  c("Reset","Reset settings"),
  c("Probability of Improvement","Set a minimum average probability of improvement"),
  c("Total Team Points","Set a minimum total team points"),
  c("Maximum Average Age","Set a maximum average team age"),
  c("Included Players","Type a list of players you must include in the roster. One from each source team please!"),
  c("Excluded Players","Type a list of players you cannot include in the roste.")
  )
)
  
colnames(help_text) <- c("Control","Effects")