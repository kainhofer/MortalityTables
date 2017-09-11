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
#' @examples
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#' # AVÖ 2005R base table with yearly improvements of 3% for age 0, linearly
#' # decreasing to 0% for age 120.
#' tb = mortalityTable.improvementFactors(
#'     ages = ages(AVOe2005R.male),
#'     deathProbs = periodDeathProbabilities(AVOe2005R.male, Period = 2002),
#'     baseYear = 2002,
#'     improvement = 0.03 * (1 - ages(AVOe2005R.male)/121),
#'     name = "AVÖ 2005R base with linearly falling improvements (DEMO)"
#' )
#' # Yearly trend is declining:
#' plotMortalityTrend(tb, AVOe2005R.male, Period = 2017, title = "Mortality Trend")
#' # The cohort tables for different birth years:
#' plot(getCohortTable(tb, YOB = 1963), getCohortTable(tb, YOB = 2017))
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
