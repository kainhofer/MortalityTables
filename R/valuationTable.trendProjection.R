#' @include valuationTable.R valuationTable.period.R
NULL

#' Class valuationTable.trendProjection - Cohort life table with age-specific trend
#'
#' A cohort life table, obtained by a trend projection from a given base table
#' (PODs for a given observation year). Typically, the trend is obtained by
#' the Lee-Carter method or some other trend estimation.
#' The dampingFunction can be used to modify the cumulative years (e.g. G(tau+x)
#' instead of tau+x)
#' If trend2 is given, the G(tau+x) gives the weight of the first trend,
#' 1-G(tau+x) the weight of the second trend
#'
#' @slot baseYear The base year of the trend projection (\code{baseTable}
#'                describes the death probabilities in this year)
#' @slot trend    The yearly improvements of the log-death probabilities (per age)
#' @slot dampingFunction A possible damping of the trend. This is a function
#'                       \code{damping(delta_years)} that gets a vector of years
#'                       from the baseYear and should return the dampened values.
#' @slot trend2   The alternate trend. If given, the damping function
#'                interpolates between \code{trend} and \code{trend2}, otherwise
#'                the dumping function simply modifies the coefficients of
#'                \code{trend}.
#'
#' @export valuationTable.trendProjection
#' @exportClass valuationTable.trendProjection
valuationTable.trendProjection = setClass(
    "valuationTable.trendProjection",
    slots = list(
        baseYear = "numeric",
        trend = "numeric",
        dampingFunction = "function",
        trend2 = "numeric"
    ),
    prototype = list(
        baseYear = 1980,
        trend = rep(0,120),
        dampingFunction = identity,
        trend2 = 0
    ),
    contains = "valuationTable.period"
)
