for (i in fs::dir_ls("R")) {source(i)}

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
             actionButton("plotrun", "Make plots"),

             uiOutput("plotinfo"),

             tabsetPanel(
               # tabPanel("test", verbatimTextOutput("p_test")),
               tabPanel("ICC",
                        plotOutput("p_icc"),
                        plotOutput("p_probp")),
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
