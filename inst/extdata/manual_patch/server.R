library(shiny)
library(miniUI)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)
setwd(.currentwd)

server <- function(input, output, session) {
  
  m <- eventReactive(input$go, {
    eval(parse(text = input$model_object))
  })
  
  res <- eventReactive(input$go, {
    start_manual_edit_unix(m())
  })
  
  # ctx <- eventReactive(input$go, {
  #   rstudioapi::getSourceEditorContext()
  # })
  
  observeEvent(input$go, {
    res()
    
    showModal(modalDialog(
      title = "Instructions",
      "1) edit control file\n
      2) save & close file\n
      3) Press \"Done\"",
      easyClose = TRUE,
      footer = NULL
    ))

    # Sys.sleep(2)  
    # ctx()
    
  })
  
  observeEvent(input$done, {
    if(input$go != 0){
      # if(normalizePath(res()$new_ctl_path) == normalizePath(ctx()$path)){
      #   rstudioapi::documentClose(ctx(), save = TRUE)
      # }
      diff_manual_edit(m(), res())
      final_text <- 
        paste0("copy-paste the following into your script to apply:\n
  [nm_object] %>%
  apply_patch(\"", res()$patch_name,"\")")
      message(final_text)
    }
    stopApp()
  })
  
}
