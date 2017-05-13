library(shiny)

fluidPage(
  navbarPage("NMproject",id="mainPanel",
    tabPanel("run database",
             actionButton("refresh_db","refresh"),
             br(),br(),
             DT::dataTableOutput("run_table"),
             actionButton("go_to_monitor","monitor"),
             actionButton("go_to_results","results")),
    tabPanel("run monitor",   # Application title
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
    tabPanel("run results")
  )
)
