library(shiny)
library(shinydashboard)
# Define UI for application that draws a histogram
shinyUI( fluidPage( 
         dashboardPage(skin = "blue",   
                       dashboardHeader(title = "Population analysis",titleWidth = 450),
                       dashboardSidebar(width = 250,
                         sidebarMenu( id = "tabs",
                           menuItem("Data", tabName = "SideB1", icon = icon("file-excel-o")),
                           menuItem("Data Exploration", tabName = "SideB2", icon = icon("line-chart")),
                           menuItem("Database", tabName = "SideB3", icon = icon("server")),
                           menuItem("Monitor", tabName = "SideB4", icon = icon("television")),
                           menuItem("Result table", tabName = "SideB5", icon = icon("tasks")),
                           menuItem("Goodness of fit", tabName = "SideB6", icon = icon("object-group")),
                           menuItem("Model creation", tabName = "SideB7", icon = icon("puzzle-piece")),
                           tags$hr(),
                           div(tags$img(src = "logo.png", width = "50%", height = "50%"), style = "text-align: center;")
                         )
                       ),
                       dashboardBody(
                         tags$head(
                           tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                         ),
                         tabItems(
                           # Data
                           tabItem(tabName = "SideB1", h2("Data"), fluidRow(
                             box(width = 3,
                               title = "Dataset properties", status = "primary",
                               fileInput("ds_ex", "Choose CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"), width = "250px"),
                               uiOutput("ui1")
                             )
                           )),
                           #Data exploration
                           tabItem(tabName = "SideB2", h2("Data Exploration"), fluidRow()
                                   
                           ),
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
                           ),
                           #Goodness of fit
                           tabItem(tabName = "SideB6", h2("GOF"), class = "active", 
                                   box(width = 3,
                                       title =  "Run(s) selected:",
                                       tags$code(textOutput("gof_select_warning")),
                                       tableOutput("runs_selected_info3"),
                                       actionButton("back_to_db3","Select different run(s)")
                                     ),
                                 
                                   
                                   fluidRow(tags$style("
                                                        .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                                                                background-color:  #FFFFFF;
                                                                border-color:  #FFFFFF;
                                                       }
                                                      
                                                     
                                                       
                                                       
                                                       "
                                                       ),                  
                                 
                                  tabBox(width=12, 
                                    tabPanel("Observed vs Prediction", plotOutput("obsvspred")),
                                    tabPanel("Weight residuals",fluidPage( uiOutput("resplot") )),
                                    tabPanel("Individual plots",fluidPage( uiOutput("indplot") )),
                                    tabPanel("Paramaters distribution")
                                   
                                         ))
                            ),
                          #Model creation
                          tabItem(tabName = "SideB7", h2("Model creation"), class = "active",
                                  
                                  box(width = 4,  
                                      title = "Model constructor", status = "primary",
                                      numericInput('Compartments',"Compartments number",2),
                                      numericInput('Parameters',"Parameters number",2),
                                      selectInput("Log_transform","Log transformation:",
                                                  c("YES" = "yes_log",
                                                      "NO"="no_log")),
                                      selectInput("MU_transform","MU transformation:",
                                                  c("YES" = "yes_mu",
                                                    "NO"="no_mu"))
                                  )     
                         )
                       ))
)))
