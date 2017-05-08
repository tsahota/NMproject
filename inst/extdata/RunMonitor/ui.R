library(shiny)

shinyUI(fluidPage(
  titlePanel("Run Monitor"),   # Application title
  fluidRow(column(2,     # Sidebar with a slider input for the number of bins
                  textInput("folder", label = h3("analysis directory"), 
                            value = gsub("^(.*)/ProjectLibrary.*$","\\1",system.file(package="NMproject")))),
           column(2,textInput("run.no", label = h3("run directory"), 
                              value = ""))),
  textOutput("runinfo"),
  tags$head(tags$style("#runinfo{color: red;
                                 font-size: 20px;
                                 }"
  )),
  textOutput("available"),
  tags$head(tags$style("#available{color: black;
                                 font-size: 20px;
                                 }"
  )),
  h3("run status"),
  tableOutput("status"),
  h3("minimisation"),
  plotOutput("distPlot")
))
