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
                 base_font = font_google("Prompt"),
                 code_font = font_google("JetBrains Mono")
                 ),

  navbarPage(

    ## CSS-Code ###############
    # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = system.file("extdata", "theme.css", package = "GRShiny"))),



    # title -------------------------------------------------
    title = "Graded response model (v.0.1)",

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
                      prettyRadioButtons("mplus",
                                         label = "Mplus?",
                                         choices = c("mplus", "lavaan"),
                                         selected = "lavaan",
                                         inline = T,
                                         bigger = TRUE),

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
                      verbatimTextOutput("results")
               )
             )
    ),
    tabPanel("Plot")


  )
)##########################  shiny UI last line  #######################
