#' @include mortalityTable.R
NULL

#' Class mortalityTable.period - Period life tables
#'
#' A period life table, giving death probabilities for each age, up to
#' maximum age \code{omega}. The \code{baseYear} slot can be used to hold
#' information about the period.
#'
#' @slot ages       The ages corresponding to the entries of the deathProbs
#' @slot deathProbs The one-year death probabilities for the ages
#'
#' @export mortalityTable.period
#' @exportClass mortalityTable.period
mortalityTable.period = setClass(
    "mortalityTable.period",
    slots = list(
        ages = "numeric",
        deathProbs = "numeric"
    ),
    prototype = list(
        ages = eval(0:120),
        deathProbs = rep(1,120)
    ),
    contains = "mortalityTable"
)
