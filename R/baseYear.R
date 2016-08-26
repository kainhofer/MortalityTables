#' @include valuationTable.R valuationTable.mixed.R
NULL

#' Return the base year of the life table
#'
#' @param object The life table object (class inherited from valuationTable)
#' @param ... Other parameters (currently unused)
#'
#' @exportMethod baseYear
setGeneric("baseYear", function(object, ...) standardGeneric("baseYear"));

#' @describeIn baseYear Return the base year of the life table
setMethod("baseYear", "valuationTable",
          function (object,  ...) {
              object@baseYear
          })

#' @describeIn baseYear Return the base year of the life table
setMethod("baseYear", "valuationTable.mixed",
          function (object,  ...) {
              baseYear(object@table1)
          })
