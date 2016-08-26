#' @include valuationTable.R valuationTable.period.R valuationTable.mixed.R valuationTable.joined.R valuationTable.observed.R
NULL

#' Return the maximum age of the life table
#'
#' @param object A life table object (instance of a \code{valuationTable} class)
#'
#' @exportMethod getOmega
setGeneric("getOmega", function(object) standardGeneric("getOmega"));

#' @describeIn getOmega Return the maximum age of the period life table
setMethod("getOmega", "valuationTable.period",
          function (object) {
              max(object@ages, na.rm = TRUE);
          })

#' @describeIn getOmega Return the maximum age of the mixed life table
setMethod("getOmega", "valuationTable.mixed",
          function (object) {
              getOmega(object@table1);
          })

#' @describeIn getOmega Return the maximum age of the joined life table
setMethod("getOmega", "valuationTable.joined",
          function (object) {
              getOmega(object@table1);
          })

#' @describeIn getOmega Return the maximum age of the joined life table
setMethod("getOmega", "valuationTable.observed",
          function (object) {
              max(object@ages, na.rm = TRUE);
          })
