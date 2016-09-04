#' @include mortalityTable.R
NULL

#' Class mortalityTable.observed - Life table from actual observations
#'
#' A cohort life table described by actual observations (data frame of PODs
#' per year and age)
#'
#' @slot data    The observations
#' @slot years   The observation years
#' @slot ages    The observation ages
#'
#' @export mortalityTable.observed
#' @exportClass mortalityTable.observed
mortalityTable.observed = setClass(
    "mortalityTable.observed",
    slots = list(
        data = "data.frame",
        years = "numeric",
        ages = "numeric"
    ),
    prototype = list(
        data = data.frame(),
        years = c(),
        ages = c()
    ),
    contains = "mortalityTable"
)
