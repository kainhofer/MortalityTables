#' @include mortalityTable.R mortalityTable.period.R mortalityTable.ageShift.R mortalityTable.trendProjection.R mortalityTable.improvementFactors.R mortalityTable.mixed.R
NULL

#' Return the mortality trend (yearly log-death-probability improvement) of the given period or the given generation.
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param ... Other parameters (currently unused)
#' @param YOB The birth year for which the mortality improvement should be calculated
#' @param Period The observation year for which the mortality improvement should
#'               be calculated. If both YOB and Period are given, YOB is ignored.
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' # AVOe 2005R includes a trend decline by default, compare the exact table
#' # with the table without decline:
#' mortalityImprovement(AVOe2005R.male, Period = 2017)
#' mortalityImprovement(AVOe2005R.male.nodamping, Period = 2017)
#'
#' @exportMethod mortalityImprovement
setGeneric("mortalityImprovement", function(object, ..., Period = NULL, YOB = 1975) standardGeneric("mortalityImprovement"));

#' @describeIn mortalityImprovement Return the yearly log-mortality improvement of the
#'                                life table given the birth or observation year
setMethod("mortalityImprovement", "mortalityTable",
          function(object, ..., Period = NULL, YOB = 1975) {
              ages = ages(object)
              if (missing(Period) || is.null(Period)) {
                  # Improvement for generation YOB
                  qt1 = deathProbabilities(object, ..., YOB = YOB - 1)
                  qt  = deathProbabilities(object, ..., YOB = YOB)
              } else {
                  # Improvement for observation year Period
                  qt1 = periodDeathProbabilities(object, ..., Period = Period - 1)
                  qt  = periodDeathProbabilities(object, ..., Period = Period)
              }
              lambda = -log(qt/qt1);
              names(lambda) = ages;
              lambda
          })

