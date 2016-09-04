#' @include mortalityTable.R mortalityTable.period.R
NULL

#' Class mortalityTable.improvementFactors - Cohort life table with improvement
#' factors
#'
#' A cohort life table, obtained by an improvment factor projection
#' from a given base table (PODs for a given observation year).
#'
#' @slot baseYear    The base year for the improvements (\code{baseTable}
#'                   describes the death probabilities in this year)
#' @slot improvement Yearly improvement factors per age
#'
#' @export mortalityTable.improvementFactors
#' @exportClass mortalityTable.improvementFactors
mortalityTable.improvementFactors = setClass(
    "mortalityTable.improvementFactors",
    slots = list(
        baseYear = "numeric",
        improvement = "numeric"
    ),
    prototype = list(
        baseYear = 2012,
        improvement = rep(0,120)
    ),
    contains = "mortalityTable.period"
)
