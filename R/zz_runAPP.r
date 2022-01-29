#' Start GRShiny
#'
#' @title This function will start GRShiny
#' @description An interactive Shiny application for running a GRM analysis.
#' @details This starts the IRT Shiny application on the users local computer.
#' @keywords IRT
#' @examples
#' \dontrun{
#' library(shiny)
#' startGRshiny()
#' }
#' @export
startGRshiny <- function(...) {
  # shiny_env <- new.env()
  # if(!missing(thedata)) {
  #   print('Setting parameters')
  #   assign('thedata', thedata, shiny_env)
  # }
  # environment(shiny_ui) <- shiny_env
  # environment(shiny_server) <- shiny_env

  if(requireNamespace("shiny", quietly = TRUE)){
    app <- shiny::shinyApp(
      ui = shiny_ui,
      server = shiny_server
    )
    shiny::runApp(app, ...)

  } else {
    stop('shiny package is not available. Please install.', call.=FALSE)
  }
}
