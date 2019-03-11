library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(scales)
library(DT)

source("lp_roster.R")
source("roster_plot.R")
source("help.R")

player <- read.csv("draft_players.csv")
nhl <- read.csv("nhl.csv")

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("united"),
  chooseSliderSkin("Flat"),
  titlePanel("", "Sockeyes Roster Optimizer"),
  sidebarLayout(
    sidebarPanel(
      
      wellPanel(
          fluidRow(
            column(4, actionButton("crunchRoster", "Compute", style = "width: 120px;", class = "btn-primary", icon = icon("paper-plane"))),
            column(1, verbatimTextOutput("placeholder")),
            column(4, downloadButton("exportRoster", "Export", style = "width: 120px;", class = "btn-primary"))
          ),
          fluidRow(
            column(9, verbatimTextOutput("placeholder1"))
          ),
          fluidRow(
            column(4, actionButton("saveSettings", "Save", style = "width: 120px;", class = "btn-primary", icon = icon("save"))),
            column(1, verbatimTextOutput("placeholder3")),
            column(4, actionButton("resetSettings", "Reset", style = "width: 120px;",class = "btn-primary", icon = icon("eraser")))
          )
        ),
      
      wellPanel(
        sliderInput("probThreshold", "Propabiliy of Improvement", min = 0, max = 1,value = 0.25),
        sliderInput("totalPTS", "Total Team Points", min = 300, max = 600, value = 450),
        sliderInput("maxAvgAge", "Maximum Average Age", min = 24, max = 40, value = 26)
      ), 
      
      wellPanel(
        fixedRow(
          column(
            10, selectInput('included_players', "Included Players:", player$Player, multiple = TRUE)
          )
        ),
        fixedRow(
          column(
            10, selectInput('excluded_players', "Excluded PLayers:", player$Player, multiple = TRUE)
          )
        )
      )
    ),
    
      mainPanel(
        tabsetPanel(
          tabPanel("Roster",
                   img(id = "logo", src = "sockeyes_logo.jpg", 
                   style = "display: block; margin-left: auto; margin-right: auto;"), 
                   tableOutput("roster_summary") , DTOutput("roster_dt")),
          tabPanel("Plot", plotOutput("plot"), plotOutput("plot1")), 
          tabPanel("Players", DTOutput("avail_players")), 
          tabPanel("Help",tableOutput("help"), 
                   img(id = "logo1", src = "sockeyes_logo.jpg", style = "width: 100px;"))
        )
      )                          
  )
)


server <- function(input, output, session) {
  roster_data <- reactiveVal(as.data.frame(c("")), label = "roster data")

  output$exportRoster <- downloadHandler(
    filename = function() {
      paste("roster-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(roster_data(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$resetSettings, {
    updateSliderInput(session, "probThreshold", value = 0.25)
    updateSliderInput(session, "totalPTS", value = 450)
    updateSliderInput(session, "maxAvgAge", value = 26)
    updateSelectInput(session, "included_players", selected = "")
    updateSelectInput(session, "excluded_players", selected = "")
  })

  observeEvent(input$saveSettings, {
    showModal(modalDialog(
      title = "Our Sincere Apologies",
      "This feature has not been implemented yet!"
    ))
  })
  
    
  observeEvent(input$crunchRoster, {
    player$Status = "Auto"
    player[player$Player %in% input$included_players, "Status"] <- "Include"
    player[player$Player %in% input$excluded_players, "Status"] <- "Exclude"
    
    roster <- make_roster(player, input$totalPTS, input$maxAvgAge, input$probThreshold)
    roster <- roster[order(roster$Player),]

    roster_data(roster)
    
    shinyjs::hide("logo")
    
    summary = rbind( 
      cbind("Total Points:", sum(roster$PTS),       "Total CAP:",   dollar(round(sum(roster$CAP)))),
      cbind("Total +/-:",    sum(roster$plusminus), "Average Age:", round(mean(roster$Age,1)))
    )

    output$roster_summary <- renderTable(
      summary, colnames = FALSE, bordered = TRUE
    ) 

    output$roster_dt <- renderDT(
      roster, options = list(pageLength=30, lengthChange = TRUE),
      rownames = FALSE
    )  

  })
  
  output$avail_players <- renderDT(
    player, options = list(pageLength=15, lengthChange = TRUE),
    rownames = FALSE
  )  
  
  output$plot <- renderPlot({
    roster_plot(nhl, roster_data())
  })
  
  output$plot1 <- renderPlot({
    player_plot(roster_data())
  })
  
  output$help <- renderTable(
    help_text, colnames = TRUE, bordered = TRUE
  ) 
}

shinyApp(ui = ui, server = server)