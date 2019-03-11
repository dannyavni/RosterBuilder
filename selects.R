fileConn<-file("select.txt","at")

for (i in 1:nrow(player)) {
  a <- paste(
    "selectInput('override_",
    i,
    "','",
    player[i,]$Player,
    "',c('Auto','Include','Exclude')),",
    sep = "")
  #selectInput("achiOverride", "Achi Maniac", c("Auto","Include","Exclude"))
  writeLines(a, fileConn)
}

close(fileConn)
