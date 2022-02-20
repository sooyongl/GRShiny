#' @include GRShiny-package.r
NULL

#' Start GRShiny
#'
#' \code{\link{startGRshiny}} is a caller function to open the Shiny interface of GRM.
#'
#' @description An interactive Shiny application for running a GRM analysis.
#' @details This starts the IRT Shiny application on the user's local computer.
#' @keywords IRT
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   startGRshiny()
#' }
#' }
#' @export
startGRshiny <- function() {
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
    shiny::runApp(app)

  } else {
    stop('shiny package is not available. Please install.', call.=FALSE)
  }
}
