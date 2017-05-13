library(shiny)

fluidPage(
  navbarPage("NMproject",id="mainPanel",
    tabPanel("database",
             actionButton("refresh_db","refresh"),
             br(),br(),
             DT::dataTableOutput("run_table"),
             textOutput("selected_runs"),
             actionButton("go_to_monitor","View in run monitor"), br(),
             actionButton("go_to_results","View in run results"),#),
             h5(verbatimTextOutput("test"))),
    tabPanel("monitor",   # Application title
             hr(),
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
    tabPanel("results",
             tableOutput("run_record"))
  )
)
