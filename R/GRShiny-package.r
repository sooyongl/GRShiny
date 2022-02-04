#' Graded response models with different estimators
#'
#' Analysis of polytomous response data using
#' unidimensional and multidimensional latent trait models under the Structure
#' Equation Modeling paradigm. Confirmatory graded response models can be estimated
#' with ML and WLS estimators. GRM data can be simulated and analyzed.
#' Finally, an interactive Shiny application for running a GRM analysis is supported.l
#'
#' @name GRShiny-package
#' @docType package
#' @title Full information maximum likelihood estimation of IRT models.
#' @author Sooyong Lee \email{sooyongl09@utexas.edu}
#' @import shiny
#' @import shinythemes
#' @import shinyWidgets
#' @import shinydashboard
#' @import bslib
#' @import lavaan
#' @import mirt
#' @import sirt
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom magrittr %>%
#' @importFrom purrr set_names
#' @importFrom purrr map
#' @importFrom data.table fread
#' @importFrom MASS mvrnorm
#' @importFrom DT renderDT
#' @importFrom DT DTOutput
#' @importFrom stats pnorm rlnorm runif step
#' @importFrom utils write.csv
#' @importFrom readr parse_number
#' @keywords package
NULL

