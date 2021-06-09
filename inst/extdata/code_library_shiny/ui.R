library(shiny)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)

setwd(.currentwd)

navbarPageWithText <- function(..., text) {
  navbar <- navbarPage(...)
  textEl <- tags$p(class = "navbar-text", text)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], textEl)
  navbar
}

fluidPage(
  navbarPageWithText("code library",id="mainPanel",
                     tabPanel(title = "code_libary",
                              value = "code_libary",
                              fluidRow(
                                "R command: ",tags$code("code_library(viewer = FALSE, return_info = TRUE)"),
                                       p(em("Click rows to select run(s)"))
                              ),
                              fluidRow(
                                actionButton("preview","preview file"),
                                actionButton("import","import file")
                              ),
                              br(),
                              fluidRow(DT::dataTableOutput("run_table"))
                     ),
                     tabPanel(title = "preview",
                              value = "preview",
                              fluidRow(
                                column(4, "R command: ",tags$code(textOutput("preview_command"))),
                                column(8, actionButton("import2","import file"))
                              ),
                              fluidRow(
                                wellPanel("File contents: ", br(), tags$code(htmlOutput("tail_lst")))
                              ),
                              column(8, actionButton("import3","import file"))
                     ),
                     text = strong("NMproject location: ",tags$code(.currentwd))
  )
)
