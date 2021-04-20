library(shiny)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)

setwd(.currentwd)

options(warn =-1)

gen_code_library <- function(){
  NMproject::code_library(viewer = FALSE, return_info = TRUE)
}

function(input, output, session) {
  session$onSessionEnded(stopApp)
  ## run table
  
  output$run_table <- DT::renderDataTable({
    gen_code_library()
  }, server = FALSE, rownames = FALSE, options = list(paging=TRUE,
                                                      searching=TRUE,
                                                      filtering=TRUE,
                                                      ordering=TRUE),
  selection = 'single'
  )
  
  objects <- eventReactive(input$run_table_rows_selected,{
    #row <- input$run_table_rows_selected
    gen_code_library()[input$run_table_rows_selected, ]
    #lapply(new_table()$entry[row],function(...)extract_nm(...,.db_name))
  })
  # 
  
  observeEvent(input$preview, {
    if(length(input$run_table_rows_selected)==0)
      return(showModal(modalDialog(
        title = "Error",
        "Select a row first"
      )))
    updateNavbarPage(session, "mainPanel", selected = "preview")
  })
  
  stage_expr <- expression({
    if(length(input$run_table_rows_selected)==0)
      return(showModal(modalDialog(
        title = "Error",
        "Select a row first"
      )))
    NMproject::stage(NMproject::ls_code_library(objects()$NAME), overwrite = TRUE)
    return(showModal(modalDialog(
      title = "file staged",
      "File is in \"staging\" directory. For scripts use import(\"staging/path/to/file\"). For nonmem code new_nm(.... , based_on = \"staging/path/to/file\", ....).  Close shiny app to go back to console."
    )))
  })
  
  observeEvent(input$stage, eval(stage_expr))
  observeEvent(input$stage2, eval(stage_expr))
  observeEvent(input$stage3, eval(stage_expr))
  
  observeEvent(input$back_to_cl, {
    updateNavbarPage(session, "mainPanel", selected = "code_library")
  })
  
  ## run monitor

  output$tail_lst <- renderUI({
    tail_ob <- readLines(file.path(objects()$FOLDER, objects()$NAME))
    tail_ob <- do.call(paste,c(as.list(tail_ob),sep='<br/>'))
    HTML(tail_ob)
  })
  
  output$preview_command <- renderText({
    paste0("preview(\"", objects()$NAME, "\")")
  })

}

