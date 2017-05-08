library(shiny)

shinyServer(function(input, output, session) {
  autoInvalidate <- reactiveTimer(20000, session)
  output$distPlot <- renderPlot({
    dir0 <- input$folder
    run_dir <- input$run_dir
    trans <- input$trans
    #if(!run.no %in% available_runs()$RUN) message("select an available run")
    orig.dir <- getwd();  setwd(dir0) ; on.exit(setwd(orig.dir))
    autoInvalidate()
    plot_iter(file.path(run.no,"NM_run1","psn.ext"),
              trans=trans)
  })
  output$status <- shiny::renderTable({
    dir0 <- input$folder
    run.no <- input$run.no
    run.no <- as.numeric(run.no)
    if(!run.no %in% available_runs()$RUN) message("select an available run")
    orig.dir <- getwd();  setwd(dir0) ; on.exit(setwd(orig.dir))
    autoInvalidate()
    status(as.numeric(run.no))
  })
  output$runinfo <- renderText({
    dir0 <- input$folder
    run.no <- input$run.no
    run.no <- as.numeric(run.no)
    if(!run.no %in% available_runs()$RUN) message("select an available run")
    paste("displaying:",file.path(dir0,run.no))
  })
  output$available <- renderText({
    dir0 <- input$folder
    orig.dir <- getwd();  setwd(dir0) ; on.exit(setwd(orig.dir))
    paste("available runs:",paste(available_runs()$RUN,collapse = ","))
  })
})

