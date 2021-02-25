library(shiny)
library(miniUI)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)
setwd(.currentwd)

# Gets The active Documeent

ui <- miniPage(
  gadgetTitleBar("select behaviour and click done"),
  miniContentPanel(
    DT::dataTableOutput("behaviour_table")#,
    # radioButtons("overwrite_behaviour", "", 
    #              NMproject::.overwrite_behaviour$txt, 
    #              selected = NMproject::overwrite_behaviour())
  )
)
