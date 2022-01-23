#' #' Launch Shiny app
#' #'
#' #' @examples
#' #' grShiny()
#' #'
#' grShiny <- function() {
#'   shinyApp(
#'     # UI app ---------------------------------------------------------------
#'     ui = navbarPage(
#'       # useShinydashboard(),
#'
#'       ## CSS-Code ###############
#'       # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = system.file("extdata", "theme.css", package = "GRShiny"))),
#'       # tags$head(
#'       #   tags$link(rel = "stylesheet", type = "text/css",
#'       #             href = file.path("inst","extdata", "theme.css"))
#'       # ),
#'
#'
#'       # title -------------------------------------------------
#'       title = "Graded response model (v.0.0.0)",
#'
#'       #---------------------------------------------------------
#'       # Data Import
#'       #---------------------------------------------------------
#'       tabPanel("Data Import",
#'                fluidRow(
#'                  column(2,
#'                         prettyRadioButtons("empirical",
#'                                            label = "Empiricial or Simulated",
#'                                            choices = c("empirical", "simulated"),
#'                                            selected = "empirical",
#'                                            status = "danger",
#'                                            icon = icon("check"),
#'                                            bigger = TRUE,
#'                                            animation = 'smooth')
#'                  ),
#'                  column(10,
#'                         uiOutput("data_import")
#'                  )
#'                )
#'       ),
#'
#'       tabPanel("Analysis",
#'                fluidRow(
#'                  column(3,
#'                         prettyCheckboxGroup("syntax", "Syntax")
#'
#'                  ),
#'                  column(3,
#'                         prettyRadioButtons("estimator",
#'                                            "Type of estimator",
#'                                            choices = c("WL", "ML"),
#'                                            selected = "WL")
#'                  )
#'
#'                ),
#'                verbatimTextOutput("results")
#'       )
#'
#'
#'     )##########################  shiny UI last line  #######################
#'     ,
#'
#'
#'     # Server app ---------------------------------------------------------------
#'     server = function(input, output, session) {
#'       #
#'       # UI ready ------------------------------------------
#'       output$data_import <- renderUI({
#'         if(input$empirical == "empirical") {
#'           fluidRow(
#'             column(4,
#'                    fileInput("setups", "Choose csv or dat file for Setup",
#'                              multiple = FALSE,
#'                              accept = c(".csv", "dat")),
#'                    actionButton("import", "IMPORT"),
#'                    br(),
#'                    div(verbatimTextOutput("validation"), id = "val")
#'             ),
#'             column(8,
#'                    tableOutput("setting"),
#'                    tableOutput("itemtable1"),
#'                    tableOutput("itemtable2"))
#'           )
#'         } else {
#'           fluidRow(
#'             column(4,
#'                    numericInput(
#'                      inputId = "npeople",
#'                      label = "Number of people",
#'                      value = 300, min = 100, max = 2000, step = 1),
#'                    numericInput(
#'                      inputId = "nitem",
#'                      label = "Number of items",
#'                      value = 10, min = 4, max = 50, step = 1),
#'                    numericInput(
#'                      inputId = "ncate",
#'                      label = "Number of categories",
#'                      value = 3, min = 2, max = 5, step = 1),
#'                    numericInput(
#'                      inputId = "nfac",
#'                      label = "Number of factors",
#'                      value = 1, min = 1, max = 4, step = 1),
#'
#'                    actionButton("import", "Import data"))
#'           )
#'         }
#'       })
#'
#'
#'       imprt_data <- eventReactive(input$import, {
#'         if(input$empirical == "empirical") {
#'           read_csv(filePath = input$setups$datapath)
#'
#'         } else {
#'           npeople <- input$npeople
#'           nitem   <- input$nitem
#'           ncate   <- input$ncate
#'           nfac    <- input$nfac
#'
#'           ipar <- genIRTpar(nitem, ncat = ncate, nfac)
#'           eta <- MASS::mvrnorm(npeople, rep(0, nfac),
#'                                # matrix(c(1), ncol = nfac),
#'                                diag(1, nfac),
#'                                empirical = T)
#'
#'           orddata <- genData(eta, ipar)
#'           orddata
#'         }
#'       })
#'
#'
#'       output$results <- renderPrint({
#'
#'         imprt_data
#'
#'
#'
#'       })
#'
#'     }##################  Shiny Server last line   ############################
#'
#'
#'   )####################  Shiny App last line   #############################
#'
#'
#'
#'
#' }
#'
#'
