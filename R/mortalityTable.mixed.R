#' @include mortalityTable.R
NULL

#' Class mortalityTable.mixed - Life table as a mix of two life tables
#'
#' A cohort life table obtained by mixing two life tables with the given weights
#'
#' @slot table1 The first \code{mortalityTable}
#' @slot table2 The second \code{mortalityTable}
#' @slot weight1 The weight of the first mortality table
#' @slot weight2 The weight of the second mortality table
#' @slot loading Additional security loading
#'
#' @export mortalityTable.mixed
#' @exportClass mortalityTable.mixed
mortalityTable.mixed = setClass(
    "mortalityTable.mixed",
    slots = c(
        table1 = "mortalityTable",
        table2 = "mortalityTable",
        weight1 = "numeric",
        weight2 = "numeric",
        loading = "numeric"
    ),
    prototype=list(
        weight1 = 1/2,
        weight2 = 1/2,
        loading = 0
    ),
    contains = "mortalityTable"
)
