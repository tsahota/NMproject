library(shiny)
library(shinyjs)

fluidPage(
  navbarPage("NMproject",id="mainPanel",
             useShinyjs(),
             tabPanel(title = "database",
                      value = "database",
                      actionButton("refresh_db","refresh"),
                      br(),br(),
                      DT::dataTableOutput("run_table"),
                      textOutput("selected_runs"),
                      actionButton("go_to_monitor","View in run monitor"), br(),
                      actionButton("go_to_results","View in run results")),
             #h5(verbatimTextOutput("test"))),
             tabPanel(title = "monitor",
                      value = "monitor",
                      actionButton("back_to_database","<< back to database"),
                      h3("status"),
                      actionButton("refresh_status","refresh"),
                      br(),br(),
                      tableOutput("status"),
                      hr(),
                      h3("trace"),
                      numericInput("skip","skip",value=0,min=0,max=1000,step=1),
                      checkboxInput("trans","transform parameters",TRUE),
                      actionButton("refresh_plot","refresh"),
                      br(),br(),
                      plotOutput("distPlot")),
             tabPanel(title = "results",
                      value = "results",
                      actionButton("back_to_database2","<< back to database"), br(),br(),
                      dataTableOutput("run_record"))
  )
)
