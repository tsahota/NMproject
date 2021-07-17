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
        "Following code can be copy-pasted to create a new parent nm object (with run id <code>\"m1\"</code>).
        <code>new_nm()</code> will fill in the $DATA (substitute <code>filename.csv</code> below for your data file).
        Using pipes and <code>fill_input()</code> can automatically fill in the $INPUT.\n
        These are just to get you started, additional modifications may been needed, 
        consult the documentation for more information:\n\n ",
        "<code>m1 <- new_nm(run_id = \"m1\", \n",
        HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
        HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
        HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
        HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
        HTML('&nbsp;'), HTML('&nbsp;'), 
        "based_on = \"", 
        import_ob$staging[!import_ob$imported],
        "\",\n",
        HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
        HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
        HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
        HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
        HTML('&nbsp;'), HTML('&nbsp;'),
        "data_path = \"",
        NMproject::nm_dir("derived_data"), .Platform$file.sep,
        "filename.csv\") %>% \n",
        HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
        HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
        "fill_input() %>%\n",
        HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
        HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'), HTML('&nbsp;'),
        "run_nm()",
        "</code>\n\n"
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
    paste0("preview(\"", objects()$NAME, "\")")
  })

}

