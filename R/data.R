#' Data for Estimating Okun's Law Across States
#'
#' Unemployment rate and real gross state product data in the United States from
#' 2018 through 2023. Data was downloaded from the Federal Reserve Economic
#' Database (FRED) using \code{\link[eFRED]{fred_state}} from the \code{eFRED}
#' package. Unemployment rates are sourced from the U.S. Bureau of Labor
#' Statistics and are associated with FRED code \code{"[state prefix]UR"}. GSP
#' data is sourced from the U.S. Bureau of Economic Analysis and are associated
#' with FRED code \code{"[state prefix]RGSP"}.
#'
#' @format A data frame with 306 rows and 4 columns:
#' \describe{
#'   \item{year}{year of the observation, 2018 - 2023}
#'   \item{state}{character vector containing the two-letter state code}
#'   \item{ur}{average annual unemployment rate (\%)}
#'   \item{gsp}{annualized growth rate (\%) of real gross state domestic product}
#' }
#'
#' @source <https://fred.stlouisfed.org/>
"okun_state"
