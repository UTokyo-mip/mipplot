#' Sample Dataset
#'
#' A sample dataset of IAMC format consist of a subset of IPCC special report (Global Warming of 1.5°C, 2018).
#'
#' @format A tibble data.table with 396425 rows and 7 variables:
#' \describe{
#'   \item{model}{model, categorical}
#'   \item{scenario}{scenario, categorical}
#'   \item{region}{region, ASIA, OECD90 or World}
#'   \item{variable}{the name of simulated variable that changes over time}
#'   \item{unit}{unit of a variable}
#'   \item{period}{year}
#'   \item{value}{the value of a variable}
#' }
#' @source \url{https://data.ene.iiasa.ac.at/iamc-1.5c-explorer/#/login?redirect=%2Fworkspaces}
"sr15_sample_data"
