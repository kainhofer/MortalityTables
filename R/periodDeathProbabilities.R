#' @include mortalityTable.R mortalityTable.period.R mortalityTable.trendProjection.R mortalityTable.improvementFactors.R mortalityTable.mixed.R
NULL

#' Return the (period) death probabilities of the life table for a given
#' observation year
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param ... Other parameters (currently unused)
#' @param Period  The observation year for which the period death probabilities should be determined
#'
#' @exportMethod periodDeathProbabilities
setGeneric("periodDeathProbabilities", function(object, ..., Period = 1975) standardGeneric("periodDeathProbabilities"));

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "mortalityTable.period",
          function(object, ..., Period = 1975) {
              object@modification(object@deathProbs * (1 + object@loading));
          })

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "mortalityTable.ageShift",
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
setMethod("periodDeathProbabilities", "mortalityTable.trendProjection",
          function(object,  ..., Period = 1975) {
              qx = object@deathProbs * (1 + object@loading);
              if (is.null(object@trend2) || length(object@trend2) <= 1) {
                  # ages = 0:(length(qx)-1);
                  damping = object@dampingFunction(Period - object@baseYear);
                  finalqx = exp(-object@trend * damping) * qx;
              } else {
                  # dampingFunction interpolates between the two trends:
                  weight = object@dampingFunction(Period);
                  finalqx = qx * exp(
                      -(object@trend * (1 - weight) + object@trend2 * weight) *
                          (Period - object@baseYear))
              }
              object@modification(finalqx)
          })

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "mortalityTable.improvementFactors",
          function(object, ..., Period = 1975) {
              qx = object@deathProbs * (1 + object@loading);
              impr = calculateImprovements(object, ..., Period = Period)
              object@modification(qx * impr)
          })

#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the life table for a given observation year
setMethod("periodDeathProbabilities", "mortalityTable.mixed",
          function(object,  ..., Period = 1975) {
              qx1 = periodDeathProbabilities(object@table1, ..., Period = Period);
              qx2 = periodDeathProbabilities(object@table2, ..., Period = Period);
              mixedqx = (object@weight1 * qx1 + object@weight2 * qx2) / (object@weight1 + object@weight2) * (1 + object@loading);
              object@modification(mixedqx)
          })

