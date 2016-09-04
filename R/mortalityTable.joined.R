#' @include mortalityTable.R
NULL

#' Class mortalityTable.joined - Life table created by joining two life tables
#'
#' A cohort life table obtained by joining two cohort life tables, each of which
#' applies only to certain observation years (e.g. for the past use the observed
#' PoDs, and project them to the future with the trend projection)
#'
#' @slot table1 The first \code{mortalityTable}, valid for years given in \code{yearRange1}
#' @slot yearRange1 The years, for which \code{table1} describes the death probabilities
#' @slot table2 The second \code{mortalityTable}, valid for years given in \code{yearRange2}
#' @slot yearRange2 The years, for which \code{table2} describes the death probabilities
#'
#' @export mortalityTable.joined
#' @exportClass mortalityTable.joined
mortalityTable.joined = setClass(
    "mortalityTable.joined",
    slots=list(
        table1 = "mortalityTable",
        yearRange1 = "numeric",
        table2 = "mortalityTable",
        yearRange2 = "numeric"
    ),
    contains = "mortalityTable"
)
