#' @include mortalityTable.R utilityFunctions.R ages.R getOmega.R periodDeathProbabilities.R deathProbabilities.R
NULL

#' Class mortalityTable.observed - Life table from actual observations
#'
#' A cohort life table described by actual observations (data frame of PODs
#' per year and age)
#'
#' @slot deathProbs    The observed death probabilities (age-specific probability of dying within one year)
#' @slot years   The observation years
#' @slot ages    The observation ages
#'
#' @export mortalityTable.observed
#' @exportClass mortalityTable.observed
mortalityTable.observed = setClass(
 "mortalityTable.observed",
 slots = list(
     deathProbs = "data.frame",
     years = "numeric",
     ages = "numeric"
 ),
 prototype = list(
     deathProbs = data.frame(),
     years = c(),
     ages = c()
 ),
 contains = "mortalityTable"
)

#' @describeIn ages Return the defined ages of the observed life table
setMethod("ages", "mortalityTable.observed",
          function(object, ...) {
              object@ages;
          })


#' @describeIn getOmega Return the maximum age of the life table
setMethod("getOmega", "mortalityTable.observed",
          function(object) {
              max(object@ages, na.rm = TRUE);
          })

#' @describeIn mT.round Return the life table with the values rounded to the given number of digits
setMethod("mT.round", "mortalityTable.observed",
          function(object, digits = 8) {
              o = callNextMethod()
              o@data = round(o@data, digits = digits)
              o
          })



# Solution to convert vector of integer values to string, with ranges for subsequent numbers:
# https://stackoverflow.com/questions/16911773/collapse-runs-of-consecutive-numbers-to-ranges
findIntRuns <- function(run) {
    rundiff <- c(1, diff(run))
    difflist <- split(run, cumsum(rundiff != 1))
    runs = unlist(lapply(difflist, function(x) {
        if (length(x) %in% 1:2) as.character(x) else paste0(x[1], "-", x[length(x)])
    }), use.names = FALSE)
    paste0(runs, collapse = ",")
}

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
#'             If the observed mortality table does not provide data
#'             for the desired period, the period closest to the
#'             `Period` argument will be used and a warning printed.
setMethod("periodDeathProbabilities", "mortalityTable.observed",
          function(object, ..., ages = NULL, Period = 1975) {
              if (is.null(ages)) {
                  ages = ages(object)
              }
              col = which.min(abs(object@years - Period))
              if (object@years[col] != Period) {
                  warning("periodDeathProbabilities: Desired Period ", Period,
                          " of observed mortalityTable not available, using closest period ",
                          object@years[[col]], ".\nAvailable periods: ", findIntRuns(object@years))
              }


              # find the given year that is closest to the desired year:
              #
              fillAges(
                  object@modification(object@deathProbs[,col] * (1 + object@loading)),
                  givenAges = ages(object),
                  neededAges = ages)
          })

#' @describeIn deathProbabilities Return the (cohort) death probabilities of the
#'                                life table given the birth year (if needed)
setMethod("deathProbabilities","mortalityTable.observed",
          function(object,  ..., ages = NULL, YOB = 1975) {
              if (is.null(ages)) {
                  ages = ages(object);
              }
              years = YOB + ages;
              yearcols = sapply(years, function(y) which.min(abs(object@years - y)))
              agerows = match(ages, object@ages)

              ## Check if all desired years are available
              if (sum(abs(object@years[yearcols] - years)) > 0) {
                  warning("deathProbabilities: Not all observation years ", findIntRuns(years),
                          " of observed mortalityTable are available, using closest observations.\nAvailable periods: ", findIntRuns(object@years))
              }

              qx = object@deathProbs[cbind(agerows, yearcols)] * (1 + object@loading);
              fillAges(object@modification(qx), givenAges = ages(object), neededAges = ages)
          })


#'@describeIn mT.cleanup Clean up the internal data of the mortality table
setMethod("mT.cleanup", "mortalityTable.observed",
          function(object) {
              o = callNextMethod()
              o@ages = unname(o@ages)
              o@deathProbs = unname(o@deathProbs)
              o@years = unname(o@years)
              o
          })
