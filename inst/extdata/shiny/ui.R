library(shiny)

fluidPage(
  tabsetPanel(id="mainPanel",
    tabPanel("run database",
             actionButton("refresh_button","refresh"),
             br(),br(),
             DT::dataTableOutput("run_table")),
    tabPanel("run monitor",   # Application title
             hr(),
             h3("status"),
             actionButton("refresh_button","update table"),
             br(),br(),
             tableOutput("status"),
             hr(),
             h3("trace"),
             numericInput("skip","skip",value=0,min=0,max=1000,step=1),
             checkboxInput("trans","transform parameters",TRUE),
             actionButton("refresh_plot","update plot"),
             br(),br(),
             plotOutput("distPlot"))
  )
)
