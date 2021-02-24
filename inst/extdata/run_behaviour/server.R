library(shiny)
library(miniUI)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)
setwd(.currentwd)

server <- function(input, output, session) {
  
  observeEvent(input$done, {
    NMproject::new_jobs(input$run_behaviour)
    stopApp()
  })

}
