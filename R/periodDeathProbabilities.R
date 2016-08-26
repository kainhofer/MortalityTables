#' @include valuationTable.R valuationTable.period.R valuationTable.trendProjection.R valuationTable.improvementFactors.R valuationTable.mixed.R
NULL

#' Return the (period) death probabilities of the life table for a given
#' observation year
#'
#' @param object The life table object (class inherited from valuationTable)
#' @param ... Other parameters (currently unused)
#' @param Period  The observation year for which the period death probabilities should be determined
#'
#' @exportMethod periodDeathProbabilities
setGeneric("periodDeathProbabilities", function(object, ..., Period = 1975) standardGeneric("periodDeathProbabilities"));

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "valuationTable.period",
          function(object, ..., Period = 1975) {
              object@modification(object@deathProbs * (1 + object@loading));
          })

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "valuationTable.ageShift",
          function (object,  ..., Period = 1975) {
              # TODO
              qx = object@deathProbs * (1 + object@loading);
              # TODO!!!
              # shift.index = match(YOB, object@shifts, 0);
              # if (shift.index) {}
              object@modification(qx)
          })

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "valuationTable.trendProjection",
          function (object,  ..., Period = 1975) {
              qx = object@deathProbs * (1 + object@loading);
              if (is.null(object@trend2) || length(object@trend2) <= 1) {
                  # ages = 0:(length(qx)-1);
                  damping = object@dampingFunction(Period - object@baseYear);
                  finalqx = exp(-object@trend * damping) * qx;
              } else {
                  # TODO!!!
                  # dampingFunction interpolates between the two trends:
                  # weights = sapply(YOB+0:(length(qx)-1), object@dampingFunction);
                  # finalqx = qx*exp(-(object@trend*(1-weights) + object@trend2*(weights))*(YOB+0:(length(qx)-1)-object@baseYear));
              }
              object@modification(finalqx)
          })

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "valuationTable.improvementFactors",
          function (object, ..., Period = 1975) {
              qx = object@deathProbs * (1 + object@loading);
              finalqx = (1 - object@improvement) ^ (Period - object@baseYear) * qx;
              object@modification(finalqx)
          })

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "valuationTable.mixed",
          function (object,  ..., Period = 1975) {
              qx1 = periodDeathProbabilities(object@table1, ..., Period = Period) * (1 + object@loading);
              qx2 = periodDeathProbabilities(object@table2, ..., Period = Period) * (1 + object@loading);
              mixedqx = (object@weight1 * qx1 + object@weight2 * qx2) / (object@weight1 + object@weight2);
              object@modification(mixedqx)
          })

