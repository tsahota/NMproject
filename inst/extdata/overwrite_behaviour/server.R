library(shiny)
library(miniUI)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)
setwd(.currentwd)

server <- function(input, output, session) {
  
  output$behaviour_table <- DT::renderDataTable(
    {
      d <- as.data.frame(NMproject::.overwrite_behaviour)
      row.names(d) <- d$txt
      d$txt <- NULL
      d
    },
    options = list(paging=FALSE,
                   searching=FALSE,
                   filtering=FALSE,
                   ordering=FALSE),
    selection = "single")
  
  txt_value <- eventReactive(input$behaviour_table_rows_selected,{
    NMproject::.overwrite_behaviour$txt[input$behaviour_table_rows_selected]
  })
  
  observeEvent(input$done, {
    NMproject::overwrite_behaviour(txt_value())
    stopApp()
  })
  
}
