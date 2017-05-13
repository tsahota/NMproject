library(shiny)

fluidPage(
  navbarPage("NMproject",id="mainPanel",
             tabPanel(title = "database",
                      value = "database",
                      actionButton("refresh_db","refresh"),
                      br(),br(),
                      DT::dataTableOutput("run_table"),
                      textOutput("selected_runs")),
             #h5(verbatimTextOutput("test"))),
             tabPanel(title = "monitor",
                      value = "monitor",
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
                      dataTableOutput("run_record"))
  )
)
