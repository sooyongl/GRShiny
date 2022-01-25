# #' #' Start GRShiny
# #' #' @title This function will start GRShiny
# #' #' @description An interactive Shiny application for running a GRM # analysis.
# #' #' @details This starts the IRT Shiny application on the users local # computer.
# #' #' @keywords IRT
# #' #' @examples
# #' #' \dontrun{
# #' #' library(shiny)
# #' #' library(shinyAce)
# #' #' library(psych)
# #' #' launchApp()
# #' #' }
# #' #' @export
# #'
# #' launchApp <- function() {
# #'
# #'   shiny::runApp(appDir = system.file("app", package="GRShiny"))
# #'
# #'appDir <- system.file("shiny-examples", "myapp", package = "mypackage")
# # if (appDir == "") {
# #   stop("Could not find example directory. Try re-installing `mypackage`.", # call. = FALSE)
# # }
# #
# # shiny::runApp(appDir, display.mode = "normal")
# #' }



# #' The Shiny App Server.
# #' @param input input set by Shiny.
# #' @param output output set by Shiny.
# #' @param session session set by Shiny.
# #' @export
# shiny_server <- function(input, output, session) {
#   if(!exists('thedata', envir = parent.env(environment()), inherits = FALSE)) {
#     message('thedata not available, using default faithful...')
#     data(faithful, envir = environment())
#     thedata <- faithful
#   }
#
#   output$environment <- renderPrint(
#     print(ls(envir = parent.env(environment())))
#   )
#
#   output$thedata <- renderTable({
#     return(thedata)
#   })
# }
#
# #' The Shiny App UI.
# #' @export
# shiny_ui <- function() {
#   fluidPage(
#     titlePanel('Shiny Parameter Test'),
#     verbatimTextOutput('environment'),
#     tableOutput('thedata')
#   )
# }


# my_shiny_app <- function(thedata, ...) {
#   shiny_env <- new.env()
#   if(!missing(thedata)) {
#     print('Setting parameters')
#     assign('thedata', thedata, shiny_env)
#   }
#   environment(shiny_ui) <- shiny_env
#   environment(shiny_server) <- shiny_env
#   app <- shiny::shinyApp(
#     ui = shiny_ui,
#     server = shiny_server
#   )
#   runApp(app, ...)
# }
