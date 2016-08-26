#' @include valuationTable.period.R valuationTable.mixed.R valuationTable.joined.R valuationTable.observed.R
NULL

#' Return the defined ages of the life table
#'
#' @param object A life table object (instance of a \code{\linkS4class{valuationTable}} class)
#' @param ... Currently unused
#'
#' @exportMethod ages
setGeneric("ages", function(object, ...) standardGeneric("ages"));

#' @describeIn ages Return the defined ages of the life table
setMethod("ages", "valuationTable.period",
          function (object, ...) {
              object@ages;
          })

# @describeIn ages Return the defined ages of the life table
setMethod("ages", "valuationTable.mixed",
          function (object, ...) {
              ages(object@table1);
          })

# @describeIn ages Return the defined ages of the life table
setMethod("ages", "valuationTable.joined",
          function (object, ...) {
              ages(object@table1);
          })

#' @describeIn ages Return the defined ages of the life table
setMethod("ages", "valuationTable.observed",
          function (object, ...) {
              object@ages;
          })

