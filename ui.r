for (i in fs::dir_ls("R")) {source(i)}

library(tidyverse)
library(lavaan)
library(mirt)
library(sirt)
library(MplusAutomation)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyalert)
library(waiter)
library(shinycssloaders)
library(bslib)
library(DT)
library(data.table)
library(officer)
library(flextable)
library(gt)
library(openxlsx)
library(readxl)

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
                      #        prettyRadioButtons("empirical",
                      #                           label = "Empiricial or Test data", # or Simulated",
                      #                           choices = c("empirical", "test"),#, "simulated"),
                      #                           selected = "empirical",
                      #                           status = "danger",
                      #                           icon = icon("check"),
                      #                           bigger = TRUE,
                      #                           animation = 'smooth')
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
                        downloadButton("report", "Download Word")
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

