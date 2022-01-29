#' @include 0_import.r
NULL

#' The Shiny App UI
#'
shiny_ui <- function() {
  fluidPage(
    # useShinydashboard(),
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css",
    #             href = file.path("inst","extdata", "theme.css"))
    # ),
    theme=bs_theme(version = 4,
                   bootswatch = "journal",
                   primary = "#ED79F9",
                   base_font = font_google("Work Sans") #
    ),

    navbarPage(

      ## CSS-Code ###############
      # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = system.file("extdata", "theme.css", package = "GRShiny"))),



      # title -------------------------------------------------
      title = "Graded response model (0.0.0)",

      #---------------------------------------------------------
      # Data Import
      #---------------------------------------------------------
      tabPanel("Data Import",
               fluidRow(
                 column(2,
                        prettyRadioButtons("empirical",
                                           label = "Empiricial or Simulated",
                                           choices = c("empirical", "simulated"),
                                           selected = "empirical",
                                           status = "danger",
                                           icon = icon("check"),
                                           bigger = TRUE,
                                           animation = 'smooth')
                 ),
                 column(10,
                        uiOutput("data_import")
                 )
               )
      ),

      tabPanel("Analysis",
               fluidRow(
                 column(3,
                        switchInput(
                          inputId = "cus_syn",
                          label = "Custom Syntax",
                          value = T,
                          onStatus = "danger",
                          offStatus = "sucess",
                          labelWidth = "100px"),

                        # prettyRadioButtons("mplus",
                        #                    label = "Mplus?",
                        #                    choices = c("mplus", "lavaan"),
                        #                    selected = "lavaan",
                        #                    inline = T,
                        #                    bigger = TRUE),

                        uiOutput("what_syn")



                 ),
                 column(2,
                        prettyRadioButtons("estimator","Type of estimator",
                                           # inline = T,
                                           choiceNames = c("WLS","MLE"),
                                           choiceValues = c("WL", "ML"),
                                           selected = "WLS"),

                        actionButton("grmrun", "Run")

                 ),
                 column(7,
                        # verbatimTextOutput("results")
                        tabsetPanel(
                          tabPanel("Model Fit",DTOutput("result0")),
                          tabPanel("Param Est",DTOutput("result1")),
                          tabPanel("GRM Param",DTOutput("result2"))
                        )
                 )
               )
      ),
      tabPanel("Plot",
               uiOutput("plotinfo"),

               tabsetPanel(
                 # tabPanel("test", verbatimTextOutput("p_test")),
                 tabPanel("ICC",

                          fluidRow(
                            column(1,
                                   materialSwitch(
                                     inputId = "blabel",
                                     label = "Add b labels",
                                     value = TRUE,
                                     status = "danger"
                                   )
                            ),
                            column(11,
                                   plotOutput("p_icc"),
                                   plotOutput("p_probp")
                            )

                          )
                 ),
                 tabPanel("EXpected Score",
                          plotOutput("p_exs")),
                 # tabPanel("Prob+", plotOutput("p_probp")),
                 tabPanel("Information",
                          plotOutput("p_info"),
                          plotOutput("p_tinfo")
                 ),
                 tabPanel("Factor Score",
                          plotOutput("p_hist"),
                          plotOutput("p_den"))
               )
      )


    )
  )##########################  shiny UI last line  #######################
}

#' The Shiny App Server
#'
#' @param input a shiny input
#' @param output a shiny output
#' @param session a seesion
#'
shiny_server <- function(input, output, session) {
  #
  # Data part ------------------------------------------
  output$data_import <- renderUI({
    if(input$empirical == "empirical") {
      fluidRow(
        column(4,
               fileInput("setups", "Choose csv or dat file for Setup",
                         multiple = FALSE, accept = c("csv", "dat")),
               numericInput("missing","Missing",value = -99,step = 1),
               textInput("chovar","Choose variables (type the column numbers)"),
               actionButton("import", "IMPORT")
        ),

        column(8,
               tabsetPanel(
                 # verbatimTextOutput("gen_data")
                 tabPanel("Imported data",DTOutput("gen_data"))
               ))
      )
    } else {
      fluidRow(
        column(2,
               numericInput(inputId = "npeople", label = "Number of people",
                            value = 300, min = 100, max = 2000, step = 1),
               numericInput(inputId = "nitem",label = "Number of items",
                            value = 10, min = 4, max = 50, step = 1),
               numericInput(inputId = "ncate",label = "Number of categories",
                            value = 3, min = 2, max = 5, step = 1),
               numericInput(inputId = "nfac",label = "Number of factors (not yet)",
                            value = 1, min = 1, max = 3, step = 1),

               actionButton("import", "Import data")),

        column(10,
               downloadButton("sim_data_down", "Download"),
               tabsetPanel(
                 tabPanel("Item parameters",DTOutput("ipar")),
                 tabPanel("Individual FS",DTOutput("indi_fs")),
                 tabPanel("Generated data",DTOutput("gen_data"))
               )
        )
      )
    }
  })

  # imprt_data <- reactiveValues()
  observeEvent(input$import, {

    if(input$empirical == "empirical"){

      orddata <- fread(input$setups$datapath)
      # if(existsFunction("imprt_data")) {
      #
      #   picked <- paste(which(names(orddata) %in% imprt_data()$varname), collapse = ",")
      # } else {
      picked <- paste0("1-", ncol(orddata))
      # }
      updateTextInput(session, "chovar", value = picked)
    }
  })

  imprt_data <- eventReactive(input$import, {
    if(input$empirical == "empirical") {
      mis_val <- input$missing
      chovar <- input$chovar

      selected_items <- eval(parse(text = paste0("c(",str_replace_all(chovar, "-", ":"),")")))

      orddata <- fread(input$setups$datapath) %>% mutate_all(~na_if(.,mis_val))
      orddata <- data.frame(orddata)[, selected_items]

      eta <- data.frame(x = "not given")
      ipar <- data.frame(x = "not given")

      list(orddata = orddata, varname = names(orddata), eta = eta, ipar = ipar)

    } else {
      npeople <- input$npeople
      nitem   <- input$nitem
      ncate   <- input$ncate
      nfac    <- input$nfac

      ipar <- genIRTpar(nitem, ncat = ncate, nfac)
      ipar <- round(ipar, 3)
      eta <- genTheta(npeople, nfac)
      eta <- round(eta, 3)
      orddata <- genData(eta, ipar)

      list(ipar=ipar, eta=eta, orddata=orddata, varname = names(orddata))
    }
  })

  output$sim_data_down <- downloadHandler(
    filename = function() {
      paste("simulated_data", ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(imprt_data()$orddata, file, row.names = F)
    }
  )

  output$ipar     <- renderDT({imprt_data()$ipar})
  output$indi_fs  <- renderDT({imprt_data()$eta})
  output$gen_data <- renderDT({imprt_data()$orddata})

  # Analysis part -----------------------------------------------------------
  output$what_syn <- renderUI({
    nfac    <- input$nfac

    orddata <- imprt_data()$orddata
    lav.model <- genLavSyn(orddata, nfac)

    if(input$cus_syn) {
      column(12,
             prettyRadioButtons("mplus", label = "Mplus?",
                                choices = c("mplus", "lavaan"),
                                selected = "lavaan",
                                inline = T, bigger = TRUE),
             textAreaInput("syntax", "Syntax",width = '100%',height = '600px')
      )
    } else {
      textAreaInput("syntax", "Syntax",width = '100%',height = '600px',value=lav.model)
    }
  })

  ## run analysis -----------------------------------------------------------
  final <- reactiveValues()
  observeEvent(input$grmrun, {
    nfac    <- input$nfac
    estimator <- input$estimator
    orddata <- imprt_data()$orddata

    if(input$cus_syn) {
      lav.model <- input$syntax

      if(input$mplus == "mplus") {

        lav.model <- lavaan::mplus2lavaan.modelSyntax(strsplit(lav.model, "\n")[[1]])
        lav.model <- str_replace(lav.model, "start\\(1\\)", "NA")
      }
    } else {
      lav.model <- genLavSyn(orddata, nfac)
    }

    final$res <- runGRM(dat = orddata, lav.syntax = lav.model, estimator = estimator)

    output$result0 <- renderDT({
      fit.dt <- extract_fit(final$res)
      fit.dt
    })

    output$result1 <- renderDT({
      extract_est(final$res) %>%
        mutate_if(is.numeric, ~ round(., 3))
    })

    output$result2 <- renderDT({

      if(!is.character(final$res$grm.par))
        round(final$res$grm.par,3)
    })

  })

  # Plot part -----------------------------------------------------------

  observeEvent(input$grmrun, {

    nfac <- input$nfac

    output$plotinfo <- renderUI({

      if(nfac != 1) {

        item_name <- names(imprt_data()$orddata)
        fluidPage(
          fluidRow(

            p("For now, not working with more than 2 factors")
          ),
          fluidRow(

            column(2,
                   pickerInput(
                     inputId = "itemchoice",
                     label = "Choose items",
                     choices = item_name,
                     selected = item_name[1:2],
                     multiple = TRUE
                   )
            ),
            column(2,
                   sliderTextInput(
                     inputId = "thetarange",
                     label = "Theta range:",
                     choices = -6:6,
                     selected = c(-5, 5)
                   )

            ),
            column(2,
                   sliderTextInput(
                     inputId = "pickcolor",
                     label = "Pick colours:",
                     choices = LETTERS[1:8],
                     selected = "D"
                   )
            ),

            column(2,
                   numericInput(
                     "linesize",
                     "Line size",
                     value = 1,
                     min = 1,
                     max = 10,
                     step = 1
                   )
            ),
            column(2,
                   numericInput(
                     "basesize",
                     "Plot text size",
                     value = 16,
                     min = 5,
                     max = 30,
                     step = 1
                   )
            ),
            column(1,
                   actionButton("plotrun", "Make plots")
            )
          )
        )
      } else {
        item_name <- names(imprt_data()$orddata)

        fluidRow(

          column(2,
                 pickerInput(
                   inputId = "itemchoice",
                   label = "Choose items",
                   choices = item_name,
                   selected = item_name[1:2],
                   multiple = TRUE
                 )
          ),
          column(2,
                 sliderTextInput(
                   inputId = "thetarange",
                   label = "Theta range:",
                   choices = -6:6,
                   selected = c(-5, 5)
                 )

          ),
          column(2,
                 sliderTextInput(
                   inputId = "pickcolor",
                   label = "Pick colours:",
                   choices = LETTERS[1:8],
                   selected = "D"
                 )
          ),

          column(2,
                 numericInput(
                   "linesize",
                   "Line size",
                   value = 1,
                   min = 1,
                   max = 10,
                   step = 1
                 )
          ),
          column(2,
                 numericInput(
                   "basesize",
                   "Plot text size",
                   value = 16,
                   min = 5,
                   max = 30,
                   step = 1
                 )
          ),
          column(1,
                 actionButton("plotrun", "Make plots")
          )
        )

      }

    })
  })
  observeEvent(input$plotrun, {

    basesize <- as.numeric(input$basesize)
    itemchoice <- input$itemchoice
    thetarange <- input$thetarange
    pickcolor <- input$pickcolor
    linesize <- input$linesize
    blabel <- input$blabel

    theta_range <- seq(thetarange[1], thetarange[2], .1)

    itemchoice <- which(imprt_data()$varname %in% itemchoice)

    res     <- final$res
    grm.par <- res$grm.par
    # fs_scores <- calFS(res)

    # output$p_test <- renderPrint({
    #
    #         itemchoice <- input$itemchoice
    #
    #         extract_numeric(itemchoice)

    # })

    output$p_icc <- renderPlot({

      p <-
        ICCplot(
          res,
          selected_item = itemchoice,
          theta = theta_range,
          plot.ps = FALSE,
          base_size = basesize,
          line_size = linesize,
          cal_option = pickcolor
        )
      p
    })

    output$p_probp <- renderPlot({
      p <-
        ICCplot(
          res,
          selected_item = itemchoice,
          theta = theta_range,
          plot.ps = T,
          base_size = basesize,
          line_size = linesize,
          cal_option = pickcolor,
          addlabel = blabel
        )
      p
    })


    output$p_exs <- renderPlot({
      p <-
        ESplot(
          res,
          selected_item = itemchoice,
          theta = theta_range,
          base_size = basesize,
          line_size = linesize,
          cal_option = pickcolor
        )
      p

    })

    output$p_info <- renderPlot({
      p <-
        infoPlot(
          res,
          selected_item = itemchoice,
          theta = theta_range,
          type = "icc",
          base_size = basesize,
          line_size = linesize,
          cal_option = pickcolor
        )
      p

    })


    output$p_tinfo <- renderPlot({
      p <-
        infoPlot(
          res,
          selected_item = itemchoice,
          theta = theta_range,
          type = "tcc",
          base_size = basesize,
          line_size = linesize,
          cal_option = pickcolor
        )
      p

    })

    output$p_hist <- renderPlot({
      p <-
        FSplot(res,
               type = "histogram",
               hist_bins = 20,
               base_size = basesize)
      p

    })

    output$p_den <- renderPlot({
      p <-
        FSplot(res,
               type = "density",
               hist_bins = 30,
               base_size = basesize)
      p

    })
  })




}##################  Shiny Server last line   ############################
