#' @include valuationTable.R
NULL

#' Class valuationTable.mixed - Life table as a mix of two life tables
#'
#' A cohort life table obtained by mixing two life tables with the given weights
#'
#' @slot table1 The first \code{valuationTable}
#' @slot table2 The second \code{valuationTable}
#' @slot weight1 The weight of the first valuation table
#' @slot weight2 The weight of the second valuation table
#' @slot loading Additional security loading
#'
#' @export valuationTable.mixed
#' @exportClass valuationTable.mixed
valuationTable.mixed = setClass(
    "valuationTable.mixed",
    slots = c(
        table1 = "valuationTable",
        table2 = "valuationTable",
        weight1 = "numeric",
        weight2 = "numeric",
        loading = "numeric"
    ),
    prototype=list(
        weight1 = 1/2,
        weight2 = 1/2,
        loading = 0
    ),
    contains = "valuationTable"
)
