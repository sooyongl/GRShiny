#' @include GRShiny-package.r
NULL

#' The Shiny App UI
#'
#' @noRd
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
                   base_font = sass::font_google("Work Sans") #
    ),

    navbarPage(

      ## CSS-Code ###############
      # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = system.file("extdata", "theme.css", package = "GRShiny"))),



      # title -------------------------------------------------
      title = "Graded response model (1.0.0)",

      #---------------------------------------------------------
      # Data Import
      #---------------------------------------------------------
      tabPanel("Data Import",
               fluidRow(
                 column(2,
                        prettyRadioButtons("empirical",
                                           label = "Simulated or Empiricial data", # or Simulated",
                                           choices = c("simulated", "empirical"),#, "simulated"),
                                           selected = "simulated",
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
                                           choiceNames = c("WLSMV","FIML"),
                                           choiceValues = c("WL", "ML"),
                                           selected = "WLSMV"),

                        actionButton("grmrun", "Run")

                 ),
                 column(7,
                        fluidRow(
                          downloadButton("rdsreport", "Download RDS"),
                          # downloadButton("report", "Download Word")
                        ),
                        tabsetPanel(
                          tabPanel("Frequency Table",
                                   # DTOutput("freq_table")
                                   gt_output("freq_table")
                          ),
                          tabPanel("Model Fit",
                                   # DTOutput("result0")
                                   gt_output("result0")
                          ),
                          tabPanel("Param Est",
                                   # DTOutput("result1")
                                   fluidRow(
                                     column(6,
                                            gt_output("result1_fl")),
                                     column(6,
                                            gt_output("result1_thre")
                                     )
                                   )
                          ),
                          tabPanel("GRM Param",
                                   # DTOutput("result2")
                                   gt_output("result2")
                          )
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
                 tabPanel("Expected Score",
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
#' @param session a session
#' @noRd
shiny_server <- function(input, output, session) {
  #
  # Data part ------------------------------------------
  output$data_import <- renderUI({
    if(input$empirical == "empirical") {
      # if(TRUE){
      fluidRow(
        column(4,
               fileInput("setups", "Choose csv or dat file for Setup",
                         multiple = FALSE, accept = c("csv", "dat")),

               checkboxInput("headery", "Variable Names", F),
               textInput("missing","Missing",value = "-99"),

               textInput("chovar","Choose variables (type the column numbers)"),
               actionButton("import", "IMPORT"),
               div(
                 checkboxInput("test_data", "Import Test Data", F),
                 style = "font-size: 10px !important; padding-top: 0px;
                   text-align:right;"
               )
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
        #
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
      if(input$test_data == F){
        # if(input$headery == "Yes") {
        #   headery == T
        # } else {
        #   headery == F
        # }
        orddata <- fread(input$setups$datapath, header = input$headery)

        # orddata <- fread(input$setups$datapath)
        # if(existsFunction("imprt_data")) {
        #
        #   picked <- paste(which(names(orddata) %in% imprt_data()$varname), collapse = ",")
        # } else {
        picked <- paste0("1-", ncol(orddata))
        # }
        updateTextInput(session, "chovar", value = picked)
      }
    }
  })

  imprt_data <- eventReactive(input$import, {

    if(input$empirical == "empirical") {
      if(input$test_data == F){

        mis_val <- input$missing
        chovar <- input$chovar

        selected_items <- eval(parse(text = paste0("c(",str_replace_all(chovar, "-", ":"),")")))

        if(mis_val != "NA") {
          orddata <- fread(input$setups$datapath) %>% mutate_all(~na_if(.,mis_val))
        } else {
          orddata <- fread(input$setups$datapath)
        }


        orddata <- data.frame(orddata)[, selected_items]

        eta <- data.frame(x = "not given")
        ipar <- data.frame(x = "not given")

        list(orddata = orddata, varname = names(orddata), eta = eta, ipar = ipar)
      } else {
        npeople <- 300 #input$npeople
        nitem   <- 10  #input$nitem
        ncate   <- 3 #input$ncate
        nfac    <- 1 #input$nfac

        ipar <- genIRTpar(nitem, ncat = ncate, nfac)
        ipar <- round(ipar, 3)
        eta <- genTheta(npeople, nfac)
        eta <- round(eta, 3)
        orddata <- genData(eta, ipar)

        list(ipar=ipar, eta=eta, orddata=orddata, varname = names(orddata))

      }

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
    nfac    <- 1 # input$nfac

    orddata <- imprt_data()$orddata
    lav.model <- genLavSyn(orddata, nfac)

    if(!input$cus_syn) {
      textAreaInput("syntax", "Syntax",width = '100%',height = '600px',value=lav.model)
    } else {

      column(12,
             prettyRadioButtons("mplus", label = "Mplus?",
                                choices = c("mplus", "lavaan"),
                                selected = "lavaan",
                                inline = T, bigger = TRUE),
             textAreaInput("syntax", "Syntax",width = '100%',height = '600px')
      )
    }
  })

  ## run analysis -----------------------------------------------------------
  final <- reactiveValues()
  observeEvent(input$grmrun, {
    nfac    <- 1 # input$nfac
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
    final$fit.dt <- extract_fit(final$res)
    final$est.dt <- extract_est(final$res)
    final$grmest.dt <- final$res$grm.par


    output$freq_table <- render_gt({ # renderDT({

      freqtable <- apply(orddata, 2, table)
      cate_name <- rownames(freqtable)
      freqtable <- data.frame(freqtable)

      freqtable %>% mutate(category = cate_name) %>%
        dplyr::select(category, dplyr::everything())

    })

    output$result0 <-render_gt({# renderDT({
      # fit.dt <- extract_fit(final$res)
      fit.dt <- final$fit.dt

      # datatable(fit.dt, rownames= FALSE)
      fittable <- fit.dt

      if(any(str_detect(fittable[[1]], "SABIC"))) {
        gt_out <- fittable %>%
          gt() %>%
          tab_header(
            title = md("**MODEL FIT INFORMATION**")
          ) %>%
          tab_row_group(
            label = md("**Information Criteria**"),
            rows = c(5:7)
          ) %>%
          tab_row_group(
            label = md("**Chi-Square Test of Model Fit**"),
            rows = c(2:4)
          ) %>%
          tab_row_group(
            label = md("**Loglikelihood**"),
            rows = 1
          )

      } else {
        gt_out <- fittable %>%
          gt() %>%
          tab_header(
            title = md("**MODEL FIT INFORMATION**")
          ) %>%
          tab_row_group(
            label = md("**SRMR (Standardized Root Mean Square Residual)**"),
            rows = c(10)
          ) %>%
          tab_row_group(
            label = md("**RMSE**"),
            rows = c(6:9)
          ) %>%
          tab_row_group(
            label = md("**CFI/TLI**"),
            rows = 4:5
          ) %>%
          tab_row_group(
            label = md("**Chi-Square Test of Model Fit**"),
            rows = c(1:3)
          )
      }

      gt_out %>%
        opt_all_caps() %>%
        #Use the Chivo font
        #Note the great 'google_font' function in 'gt' that removes the need to pre-load fonts
        opt_table_font(
          font = list(
            google_font("Chivo"),
            default_fonts()
          )
        ) %>%
        tab_style(
          locations = cells_column_labels(columns = gt::everything()),
          style     = list(
            #Give a thick border below
            cell_borders(sides = "bottom", weight = px(3)),
            #Make text bold
            cell_text(weight = "bold")
          )
        ) %>%
        tab_options(row_group.background.color = "orange")

    })

    output$result1_fl <- render_gt({ #renderDT({
      est_results <- final$est.dt %>%
        mutate_if(is.numeric, ~ round(., 3)) %>%
        filter(str_detect(op, "=~"))
      # datatable(., rownames= FALSE)

      # est_results <- extract_est(a1) %>%
      #   mutate_if(is.numeric, ~ round(., 3))

      fl <- str_which(est_results$op, "=~")
      thre <- str_which(est_results$rhs, "^t")

      est_results %>%
        gt() %>%
        opt_all_caps() %>%
        #Use the Chivo font
        #Note the great 'google_font' function in 'gt' that removes the need to pre-load fonts
        opt_table_font(
          font = list(
            google_font("Chivo"),
            default_fonts()
          )
        ) %>%
        tab_style(
          locations = cells_column_labels(columns = gt::everything()),
          style     = list(
            #Give a thick border below
            cell_borders(sides = "bottom", weight = px(3)),
            #Make text bold
            cell_text(weight = "bold")
          )
        ) %>%
        tab_header(
          title = md("**MODEL RESULTS**")
        ) %>%
        tab_row_group(
          label = md("**Item Slope**"),
          rows = c(fl)
        ) %>%
        tab_options(row_group.background.color = "orange")
    })


    output$result1_thre <- render_gt({ #renderDT({
      est_results <- final$est.dt %>%
        mutate_if(is.numeric, ~ round(., 3)) %>%
        filter(str_detect(rhs, "^t"))
      # datatable(., rownames= FALSE)

      # est_results <- extract_est(a1) %>%
      #   mutate_if(is.numeric, ~ round(., 3))

      fl <- str_which(est_results$op, "=~")
      thre <- str_which(est_results$rhs, "^t")

      est_results %>%
        gt() %>%
        opt_all_caps() %>%
        #Use the Chivo font
        #Note the great 'google_font' function in 'gt' that removes the need to pre-load fonts
        opt_table_font(
          font = list(
            google_font("Chivo"),
            default_fonts()
          )
        ) %>%
        tab_style(
          locations = cells_column_labels(columns = gt::everything()),
          style     = list(
            #Give a thick border below
            cell_borders(sides = "bottom", weight = px(3)),
            #Make text bold
            cell_text(weight = "bold")
          )
        ) %>%
        tab_header(
          title = md("**MODEL RESULTS**")
        ) %>%
        tab_row_group(
          label = md("**Thresholds**"),
          rows = c(thre)
        ) %>%
        tab_options(row_group.background.color = "orange")
    })


    output$result2 <- render_gt({  #renderDT({

      varname <- final$est.dt %>% filter(str_detect(op, "=~")) %>% pull(rhs)

      # if(!is.character(final$res$grm.par))
      out_gt <- final$res$grm.par %>% data.frame() %>%
        mutate_if(is.numeric, ~ round(.x,3)) %>%
        mutate(indicator = varname) %>%
        dplyr::select(indicator, dplyr::everything())

      out_gt %>%
        gt() %>%
        tab_header(
          title = md("**GRM PARAMETERS**")
        ) %>%
        tab_style(
          locations = cells_column_labels(columns = gt::everything()),
          style     = list(
            #Give a thick border below
            cell_borders(sides = "bottom", weight = px(3)),
            #Make text bold
            cell_text(weight = "bold")
          )
        ) %>%
        opt_all_caps() %>%
        #Use the Chivo font
        #Note the great 'google_font' function in 'gt' that removes the need to pre-load fonts
        opt_table_font(
          font = list(
            google_font("Chivo"),
            default_fonts()
          )
        ) %>%
        tab_options(row_group.background.color = "orange")
    })


    ### downloading data -------------------------------------------
    output$rdsreport <- downloadHandler(
      filename = function() {
        paste("Report_",Sys.Date(), ".rds", sep = "")
      },

      content = function(file) {
        shiny::withProgress(
          message = paste0("Downloading", " the document"),
          value = 0,
          {
            incProgress(1/10);Sys.sleep(1);incProgress(5/10)
            outlist <- list()
            outlist$fit <- final$fit.dt
            outlist$est <- final$est.dt
            outlist$grmest <- final$grmest.dt

            saveRDS(outlist, file)

            Sys.sleep(1);incProgress(4/10);Sys.sleep(1)
          }
        )
      }
    )

    output$report <- downloadHandler(
      filename = function() {
        # paste("Report_",Sys.Date(), ".docx", sep = "")
        paste("Report_",Sys.Date(), ".xlsx", sep = "")
      },

      content = function(file) {
        shiny::withProgress(
          message = paste0("Downloading", " the document"),
          value = 0,
          {
            incProgress(1/10);Sys.sleep(1);incProgress(5/10)

            # file_docx <- tempfile(fileext = ".docx")
            # word_out3(
            #   filename = file_docx,
            #   reportTables = final)
            # file.rename( from = file_docx, to = file )

            file_excel <- tempfile(fileext = ".xlsx")
            file_xlxs <- excel_out3(
              filename = file_excel,
              reportTables = final)

            saveWorkbook(file_xlxs, file=file, overwrite = T)

            Sys.sleep(1);incProgress(4/10);Sys.sleep(1)
          }
        )
      }
    )



  })

  # Plot part -----------------------------------------------------------

  observeEvent(input$grmrun, {

    nfac    <- 1 # input$nfac

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
          plot.occ = FALSE,
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
          plot.occ = T,
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

