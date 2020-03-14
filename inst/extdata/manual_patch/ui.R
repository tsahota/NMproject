library(shiny)
library(miniUI)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)
setwd(.currentwd)

# Gets The active Documeent
ctx <- rstudioapi::getActiveDocumentContext()
selected_text <- ctx$selection[[1]]$text

ui <- miniPage(
  gadgetTitleBar("Manual Edit"),
  miniContentPanel(
    textInput("model_object", "model object", value = selected_text),
    actionButton("go", "start edit"),
    textOutput("out_text"),
    textOutput("final_text")
  )
)
