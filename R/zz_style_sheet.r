#' #' @include 0_import.r
#'
#' # https://stackoverflow.com/questions/56042133/where-to-store-ccs-file-when-packaging-a-shiny-app
#'
#' .onLoad <- function(...) {
#'   shiny::addResourcePath(
#'     prefix = "custom-assets", # custom prefix that will be used to reference your directory
#'     directoryPath = system.file("www", package = "emstan") # path to resource in your package
#'   )
#' }
