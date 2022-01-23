for (i in fs::dir_ls("R")) {source(i)}

fluidPage(
  # useShinydashboard(),
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css",
  #             href = file.path("inst","extdata", "theme.css"))
  # ),
  theme=bs_theme(version = 4, bootswatch = "default"),

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
                      prettyCheckboxGroup("syntax", "Syntax")

               ),
               column(3,
                      prettyRadioButtons("estimator",
                                         "Type of estimator",
                                         choices = c("WL", "ML"),
                                         selected = "WL")
               )

             ),
             verbatimTextOutput("results")
    )


  )
)##########################  shiny UI last line  #######################
