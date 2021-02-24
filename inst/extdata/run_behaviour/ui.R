library(shiny)
library(miniUI)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)
setwd(.currentwd)

# Gets The active Documeent
ctx <- rstudioapi::getSourceEditorContext()
selected_text <- ctx$selection[[1]]$text

ui <- miniPage(
  gadgetTitleBar("Run behaviour"),
  miniContentPanel(
    selectInput("run_behaviour", "run behaviour", 
                c("run", "stop", "skip"), 
                selected = NMproject::new_jobs())
  )
)
