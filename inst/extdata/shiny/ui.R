library(shiny)
library(shinydashboard)
# Define UI for application that draws a histogram
shinyUI( dashboardPage(skin = "blue",
                       dashboardHeader(title = "NMproject",titleWidth = 450),
                       dashboardSidebar(width = 150,
                                        sidebarMenu( id = "tabs",
                                      menuItem("Database", tabName = "SideB3", icon = icon("server")),
                                      menuItem("Monitor", tabName = "SideB4", icon = icon("television")),
                                      menuItem("Result table", tabName = "SideB5", icon = icon("tasks")),

                                      tags$hr()
                         )
                       ),
                       dashboardBody(
                         tags$head(
                           tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                         ),
                         tabItems(

                           #List with models and text editor
                           tabItem(tabName = "SideB3", h2("Database"), #fluidRow()
                                   tabPanel(title = "database",
                                            value = "database",
                                            fluidRow(
                                              column(4,"R command: ",tags$code("run_table()"),
                                                     actionButton("refresh_db","refresh"),
                                                     p(em("Click rows to select run(s)"))),
                                              column(8,
                                                     actionButton("run_job","Run asynchronously"),
                                                     actionButton("go_to_monitor","View selected run in monitor tab"),
                                                     actionButton("go_to_results","View selected run(s) in results tab"))),
                                            fluidRow(DT::dataTableOutput("run_table")))
                           ),
                           # Fitting monitor
                           tabItem(tabName = "SideB4", h2("Monitor"), fluidRow(
                             column(3,wellPanel("Run selected:",
                                                tags$code(textOutput("monitor_select_warning")),
                                                tableOutput("runs_selected_info"),
                                                actionButton("back_to_db","Select different run")),
                                    "R command: ",tags$code("plot_iter([nm object])"),
                                    checkboxInput("trans","transform parameters",TRUE),
                                    actionButton("refresh_plot","refresh")),
                             column(9,wellPanel("R command: ",tags$code("status([nm object])"),
                                                actionButton("refresh_status","refresh"),br(),
                                                tags$code(verbatimTextOutput("status")),br(),
                                                tags$code(htmlOutput("tail_lst"))))),

                             uiOutput("distPlot")

                           ),
                           #tables with fitting results
                           tabItem(tabName = "SideB5", h2("Result table"),
                                   fluidRow(
                                     column(3,wellPanel("Run(s) selected:",
                                                        tags$code(textOutput("results_select_warning")),
                                                        tableOutput("runs_selected_info2"),
                                                        actionButton("back_to_db2","Select different run(s)"))),
                                     column(9,"R command: ",tags$code("run_record([nm object(s)])"),
                                            actionButton("refresh_run_record","refresh"),
                                            DT::dataTableOutput("run_record")))
                           )
                         ))
))
