#' @include valuationTable.ageShift.R
NULL

#' Return the age shift of the age-shifted life table given the birth year
#'
#' @param object The life table object (class inherited from valuationTable)
#' @param ... Other parameters (currently unused)
#' @param YOB The birth year for which the age shift should be determined.
#'
#' @exportMethod ageShift
setGeneric("ageShift", function(object, YOB=1975, ...) standardGeneric("ageShift"));

#' @describeIn ageShift Return the age shift of the age-shifted life table
#'                      given the birth year
setMethod("ageShift",
          "valuationTable.ageShift",
          function(object, YOB, ...) {
              shift = object@ageShifts[toString(YOB),];
              if (is.na(shift)) {
                  # The row names (YOB) are unfortunately strings, so we cannot easily query them.
                  # TODO: Change the data.frame to use a real column for the YOB
                  firstYOB = utils::head(rownames(object@ageShifts), n = 1);
                  lastYOB = utils::tail(rownames(object@ageShifts), n = 1);
                  if (YOB < as.integer(firstYOB)) {
                      shift = object@ageShifts[firstYOB,];
                  } else if (YOB > as.integer(lastYOB)) {
                      shift = object@ageShifts[lastYOB,];
                  }
              }
              shift
          })

