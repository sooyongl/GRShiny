shinyServer(
  function(input, output, session) {
    #
    # Data part ------------------------------------------
    output$data_import <- renderUI({
      if(input$empirical == "empirical") {
        fluidRow(
          column(2,
                 fileInput("setups", "Choose csv or dat file for Setup",
                           multiple = FALSE, accept = c("csv", "dat")),
                 numericInput("missing","Missing",value = -99,step = 1),
                 # uiOutput("chovarUI"),
                 # textInput("chovar","Choose variables (type the column numbers)"),
                 actionButton("import", "IMPORT")
          ),
          column(10,
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
                 numericInput(inputId = "nfac",label = "Number of factors (not yet)",
                              value = 1, min = 1, max = 1, step = 1),





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
    # eventReactive(input$import, {
    # output$chovarUI <- renderUI({
    #
    #   orddata <- fread(input$setups$datapath)
    #
    #   textInput("chovar","Choose variables (type the column numbers)")
    # })
    # })

    imprt_data <- eventReactive(input$import, {
      if(input$empirical == "empirical") {

        mis_val <- input$missing
        # chovar <- input$chovar

        # if(grepl("-", chovar)) {
        #
        #
        # }
        #
        # selected_items <- eval(parse(text = str_replace(chovar, "-", ":")))

        orddata <- fread(input$setups$datapath) %>% mutate_all(~na_if(.,mis_val))
        # orddata <- orddata[,selected_items]


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

    output$sim_data_down <- downloadHandler(
      filename = function() {
        paste("simulated_data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(imprt_data()$orddata, file, row.names = F)
      }
    )

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
    final <- reactiveValues()
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

      final$res <- runGRM(dat = orddata, lav.syntax = lav.model, estimator = estimator)

      # output$results <- renderPrint({
      #   if(estimator == "WL") {
      #     list(SEM_par = lavaan_out_cleaning(final$res$lav.fit),
      #          GRM_par = round(final$res$grm.par,3))
      #   } else {
      #     list(SEM_par = mirt_out_cleaning(final$res$mirt.fit),
      #          GRM_par = round(final$res$grm.par,3))
      #   }
      # })

      output$result0 <- renderDT({
        fit.dt <- extract_fit(final$res)
        fit.dt
      })

      output$result1 <- renderDT({

        output_cleaning(final$res) %>%
          mutate_if(is.numeric, ~ round(., 3))

        # if(estimator == "WL") {
        #   lavaan_out_cleaning(final$res$lav.fit) %>%
        #     mutate_if(is.numeric, ~ round(., 3))
        # } else {
        #   lavaan_out_cleaning(final$res$mirt.fit) %>%
        #     mutate_if(is.numeric, ~ round(., 3))
        # }
      })

      output$result2 <- renderDT({
        round(final$res$grm.par,3)
      })

    })

    # Plot part -----------------------------------------------------------

    observeEvent(input$grmrun, {
      output$plotinfo <- renderUI({
        item_name <- names(imprt_data()$orddata)

        fluidRow(
          column(2,
                 pickerInput(
                   inputId = "itemchoice",
                   label = "Choose items",
                   choices = item_name,
                   multiple = TRUE
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
          )
        )

      })
    })
    observeEvent(input$plotrun, {

      basesize <- as.numeric(input$basesize)
      itemchoice <- input$itemchoice
      itemchoice <- as.numeric(parse_number(itemchoice))

      res     <- final$res
      grm.par <- res$grm.par
      fs_scores <- calFS(res)

      # output$p_test <- renderPrint({
      #
      #         itemchoice <- input$itemchoice
      #
      #         extract_numeric(itemchoice)

      # })

      output$p_icc <- renderPlot({

        p <-
          ICCplot(
            grm.par,
            selected_item = itemchoice,
            theta = seq(-3, 3, .1),
            plot.ps = FALSE,
            base_size = basesize
          )
        p
      })

      output$p_probp <- renderPlot({
        p <-
          ICCplot(
            grm.par,
            selected_item = itemchoice,
            theta = seq(-3, 3, .1),
            plot.ps = T,
            base_size = basesize
          )
        p
      })


      output$p_exs <- renderPlot({
        p <-
          ESplot(
            grm.par,
            selected_item = itemchoice,
            theta = seq(-3, 3, .1),
            base_size = basesize
          )
        p

      })

      output$p_info <- renderPlot({
        p <-
          infoPlot(
            grm.par,
            selected_item = itemchoice,
            type = "icc",
            base_size = basesize
          )
        p

      })


      output$p_tinfo <- renderPlot({
        p <-
          infoPlot(
            grm.par,
            selected_item = itemchoice,
            type = "tcc",
            base_size = basesize
          )
        p

      })

      output$p_hist <- renderPlot({
        p <-
          FSplot(fs_scores,
                 type = "histogram",
                 hist_bins = 20,
                 base_size = basesize)
        p

      })

      output$p_den <- renderPlot({
        p <-
          FSplot(fs_scores,
                 type = "density",
                 hist_bins = 30,
                 base_size = basesize)
        p

      })
    })




  }##################  Shiny Server last line   ############################
)
