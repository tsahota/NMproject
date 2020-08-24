library(shiny)
library(miniUI)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)
setwd(.currentwd)

# Gets The active Documeent
ctx <- rstudioapi::getSourceEditorContext()
selected_text <- ctx$selection[[1]]$text

ui <- miniPage(
  gadgetTitleBar("Manual Edit"),
  miniContentPanel(
    textInput("model_object", "base model object", value = selected_text),
    textInput("patch_name", "(optional) patch to replace"),
    actionButton("go", "start edit"),
    br(),
    textOutput("out_text"),
    textOutput("final_text")
  )
)
