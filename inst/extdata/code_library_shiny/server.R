library(shiny)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)

setwd(.currentwd)

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
    gen_code_library()[input$run_table_rows_selected, ]
  })
  
  observeEvent(input$preview, {
    if(length(input$run_table_rows_selected)==0)
      return(showModal(modalDialog(
        title = "Error",
        "Select a row first"
      )))
    updateNavbarPage(session, "mainPanel", selected = "preview")
  })
  
  import_expr <- expression({
    if(length(input$run_table_rows_selected)==0)
      return(showModal(modalDialog(
        title = "Error",
        "Select a row first"
      )))

    stage_ob <- NMproject::stage(
      NMproject::ls_code_library(objects()$NAME), 
      overwrite = TRUE,
      silent = TRUE
    )
    import_ob <- NMproject::import(stage_ob, silent = TRUE)
    
    destinations <- ifelse(import_ob$imported, 
                           import_ob$destination, 
                           import_ob$staging)
    
    new_nm_text <- character()
    if(any(!import_ob$imported)) 
      new_nm_text <- paste0(
        "Create nm objects based on this file with:\n ",
        "<code>new_nm(.... , based_on = \"", 
        import_ob$staging[!import_ob$imported],
        "\", ....)</code>\n\n"
      )
    
    popup_text <- paste0(
      "File has been imported into following location:\n ",
      "<code>",paste0(destinations, collapse = " \n"), "</code>\n\n",
      new_nm_text,
      "Close browser to go back to RStudio or press \"Dismiss\" to import more"
    )

    popup_text <- HTML(gsub("\n", "<br>", popup_text))
    
    return(showModal(modalDialog(
      title = "file imported", popup_text
    )))
    
  })
  
  observeEvent(input$import, eval(import_expr))
  observeEvent(input$import2, eval(import_expr))
  observeEvent(input$import3, eval(import_expr))
  
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
    paste0("deprecated")
  })

}

