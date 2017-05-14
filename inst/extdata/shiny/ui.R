library(shiny)

mycss <- "
#plot-container {
  position: relative;
}
#loading-spinner {
  position: absolute;
  left: 50%;
  top: 50%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
}
#table-container {
  position: relative;
}
#loading-spinner2 {
  position: absolute;
  left: 10%;
  top: 50%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#table.recalculating {
  z-index: -2;
}
"

navbarPageWithText <- function(..., text) {
  navbar <- navbarPage(...)
  textEl <- tags$p(class = "navbar-text", text)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], textEl)
  navbar
}

fluidPage(
  tags$head(tags$style(HTML(mycss))),
  navbarPageWithText("NMproject",id="mainPanel",
                     tabPanel(title = "database",
                              value = "database",
                              fluidRow(column(6,"R command: ",tags$code("run_table()"),
                                              actionButton("refresh_db","refresh"),
                                              p(em("Click rows to select run(s)"))),
                                       column(6,actionButton("go_to_monitor","View selected run in monitor tab"),
                                              actionButton("go_to_results","View selected run(s) in monitor tab"))),
                              fluidRow(DT::dataTableOutput("run_table"))),
                     tabPanel(title = "monitor",
                              value = "monitor",
                              "R command: ",tags$code("status([nm object])"),
                              actionButton("refresh_status","refresh"),
                              br(),br(),
                              tableOutput("status"),
                              hr(),
                              "R command: ",tags$code("plot_iter([nm object])"),
                              numericInput("skip","skip",value=0,min=0,max=1000,step=1),
                              checkboxInput("trans","transform parameters",TRUE),
                              actionButton("refresh_plot","refresh"),
                              br(),br(),
                              div(id = "plot-container",
                                  tags$img(src = "spinner.gif",
                                           id = "loading-spinner"),
                                  plotOutput("distPlot")
                              )),
                     tabPanel(title = "results",
                              value = "results",
                              "R command: ",tags$code("run_record([nm object(s)])"),
                              actionButton("refresh_run_record","refresh"),
                              DT::dataTableOutput("run_record")),
                     text = strong("NMproject location: ",tags$code(.currentwd))
  )
)
