library(shiny)
library(dygraphs)
library(NMproject)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)
.m <- get(".m", envir = NMproject:::.sso_env)

setwd(.currentwd)

navbarPageWithText <- function(..., text) {
  navbar <- navbarPage(...)
  textEl <- tags$p(class = "navbar-text", text)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], textEl)
  navbar
}

fluidPage(
  navbarPageWithText("NMproject",id="mainPanel",
                     tabPanel(title = "database",
                              value = "database",
                              fluidRow(
                                column(4,"R command: ",tags$code("nm_row()"),
                                       actionButton("refresh_db","refresh"),
                                       p(em("Click rows to select run(s)"))),
                                column(8,
                                       actionButton("go_to_monitor","View selected run in monitor tab"),
                                       actionButton("go_to_results","View selected run(s) in results tab"))
                              ),
                              fluidRow(DT::dataTableOutput("run_table"))
                     ),
                     tabPanel(title = "monitor",
                              value = "monitor",
                              fluidRow(
                                column(3,
                                       wellPanel("Run selected:",
                                                 tags$code(textOutput("monitor_select_warning")),
                                                 tableOutput("runs_selected_info"),
                                                 actionButton("back_to_db","Select different run")
                                       ),
                                       "R command: ",tags$code("plot_iter([nm object])"),
                                       checkboxInput("trans","transform parameters",TRUE),
                                       actionButton("refresh_plot","refresh")
                                ),
                                column(9,
                                       wellPanel("R command: ",tags$code("status([nm object])"),
                                                 actionButton("refresh_status","refresh"),br(),
                                                 tags$code(verbatimTextOutput("status")),br(),
                                                 tags$code(htmlOutput("tail_lst"))
                                       )
                                )
                              ),
                              #plotlyOutput("distPlot",width = "auto")
                              #plotOutput("distPlot")
                              uiOutput("distPlot")
                     ),
                     tabPanel(title = "results",
                              value = "results",
                              fluidRow(
                                column(3,wellPanel("Run(s) selected:",
                                                   tags$code(textOutput("results_select_warning")),
                                                   tableOutput("runs_selected_info2"),
                                                   actionButton("back_to_db2","Select different run(s)"))
                                ),
                                column(9,"R command: ",tags$code("rr([nm object(s)])"),
                                       actionButton("refresh_run_record","refresh"),
                                       DT::dataTableOutput("run_record")
                                )
                              )
                     ),
                     text = strong("NMproject location: ",tags$code(.currentwd))
  )
)
