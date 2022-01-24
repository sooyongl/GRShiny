shinyServer(
  function(input, output, session) {
    #
    # Data part ------------------------------------------
    output$data_import <- renderUI({
      if(input$empirical == "empirical") {
        fluidRow(
          column(4,
                 fileInput("setups", "Choose csv or dat file for Setup",
                           multiple = FALSE, accept = c("csv", "dat")),
                 numericInput("missing","Missing",value = -99,step = 1),
                 actionButton("import", "IMPORT")
          ),
          column(8,
                 tabsetPanel(
                   tabPanel("Generated data",DTOutput("gen_data")),
                   tabPanel("Item parameters",DTOutput("ipar")),
                   tabPanel("Individual FS",DTOutput("indi_fs"))
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
                 numericInput(inputId = "nfac",label = "Number of factors",
                              value = 1, min = 1, max = 1, step = 1),
                 actionButton("import", "Import data")),

          column(10,
                 tabsetPanel(
                   tabPanel("Item parameters",DTOutput("ipar")),
                   tabPanel("Individual FS",DTOutput("indi_fs")),
                   tabPanel("Generated data",DTOutput("gen_data"))
                 )
          )
        )
      }
    })


    imprt_data <- eventReactive(input$import, {
      if(input$empirical == "empirical") {

        mis_val <- input$missing

        orddata <- fread(input$setups$datapath) %>% mutate_all(~na_if(.,mis_val))

        eta <- data.frame(x = "not given")
        ipar <- data.frame(x = "not given")

        list(orddata = orddata, eta = eta, ipar = ipar)

      } else {
        npeople <- input$npeople
        nitem   <- input$nitem
        ncate   <- input$ncate
        nfac    <- input$nfac

        ipar <- genIRTpar(nitem, ncat = ncate, nfac)
        ipar <- round(ipar, 3)
        eta <- MASS::mvrnorm(npeople, rep(0, nfac),
                             # matrix(c(1), ncol = nfac),
                             diag(1, nfac), empirical = T)
        eta <- round(eta, 3)
        orddata <- genData(eta, ipar)

        list(ipar=ipar, eta=eta, orddata=orddata)
      }
    })

    output$ipar     <- renderDT({imprt_data()$ipar})
    output$indi_fs  <- renderDT({imprt_data()$eta})
    output$gen_data <- renderDT({imprt_data()$orddata})

    # Analysis part -----------------------------------------------------------
    output$what_syn <- renderUI({

      orddata <- imprt_data()$orddata
      lav.model <- genLavSyn(orddata)

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
    observeEvent(input$grmrun, {

      estimator <- input$estimator
      orddata <- imprt_data()$orddata

      if(input$cus_syn) {
        lav.model <- input$syntax

        if(input$mplus == "mplus") {

          lav.model <- lavaan::mplus2lavaan.modelSyntax(strsplit(lav.model, "\n")[[1]])
          lav.model <- str_replace(lav.model, "start\\(1\\)", "NA")
        }
      } else {
        lav.model <- genLavSyn(orddata)
      }

      res <- runGRM(dat = orddata, lav.syntax = lav.model, estimator = estimator)

      output$results <- renderPrint({
        if(estimator == "WL") {
          list(SEM_par = lavaan_out_cleaning(res$lav.fit),
               GRM_par = round(res$grm.par,3))
        } else {
          list(SEM_par = mirt_out_cleaning(res1$mirt.fit),
               GRM_par = round(res$grm.par,3))
        }
      })
    })

    # Plot part -----------------------------------------------------------





  }##################  Shiny Server last line   ############################
)
