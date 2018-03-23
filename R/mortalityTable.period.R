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
#' @slot exposures  (Optional) exposured used to determine death probabilities
#'                  (can be used as weights for smoothing, for variances, etc.)
#'
#' @export mortalityTable.period
#' @exportClass mortalityTable.period
mortalityTable.period = setClass(
    "mortalityTable.period",
    slots = list(
        ages = "numeric",
        deathProbs = "numeric",
        exposures = "numeric"
    ),
    prototype = list(
        ages = eval(0:120),
        deathProbs = rep(1,120),
        exposures = NULL
    ),
    contains = "mortalityTable"
)

#' Generate a mortality table with all probabilities set to zero.
#'
#' @param name The name of the table
#' @param ages The ages of the table
#'
#' @export
mortalityTable.zeroes = function(name = "Zero mortality table", ages = 0:99) {
    mortalityTable.period(name = name, ages = ages, deathProbs = ages * 0)
}

#' Generate a (deterministic) mortality table with only one probability set to 1 (for the given age)
#'
#' @param transitionAge The age where the deterministic transition occurs
#' @param name The name of the table
#' @param ages The ages of the table
#'
#' @export
mortalityTable.once = function(transitionAge, name = "Deterministic mortality table", ages = 0:99) {
    mortalityTable.period(
        name = name,
        ages = ages,
        deathProbs = sapply(ages, function(x) { if (x == transitionAge) 1 else 0})
    )
}

#' Generate a (deterministic) mortality table with all probabilities starting at a given age set to 1
#'
#' @param transitionAge The age where the deterministic transition occurs
#' @param name The name of the table
#' @param ages The ages of the table
#'
#' @export
mortalityTable.onceAndFuture = function(transitionAge, name = "Deterministic mortality table", ages = 0:99) {
    mortalityTable.period(
        name = name,
        ages = ages,
        deathProbs = sapply(ages, function(x) { if (x >= transitionAge) 1 else 0})
    )
}

