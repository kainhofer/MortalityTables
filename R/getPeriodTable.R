#' @include valuationTable.R
NULL

#' Return the period life table as a \code{valuationTable.period} object
#'
#' @param object The life table object (class inherited from valuationTable)
#' @param Period The observation year, for which the death probabilities should
#'        be determined
#' @param ... Other parameters (currently unused)
#'
#' @exportMethod getPeriodTable
setGeneric("getPeriodTable",
           function(object, Period, ...)
               standardGeneric("getPeriodTable")
);

#' @describeIn getPeriodTable Return the period life table as a
#'             \code{valuationTable.period} object
setMethod("getPeriodTable","valuationTable",
          function (object, Period, ...) {
              valuationTable.period(
                  name = paste(object@name, ", Period ", Period),
                  baseYear = Period,
                  ages = ages(object),
                  deathProbs = periodDeathProbabilities(object, Period = Period)
              )
          })
