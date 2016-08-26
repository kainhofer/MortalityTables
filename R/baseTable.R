#' @include valuationTable.R valuationTable.period.R
NULL

#' Return the base table of the life table
#'
#' @param object The life table object (class inherited from valuationTable)
#' @param ... Other parameters (currently unused)
#'
#' @exportMethod baseTable
setGeneric("baseTable", function(object, ...) standardGeneric("baseTable"));

#' @describeIn baseTable Return the base table of the life table
setMethod("baseTable", "valuationTable",
          function (object,  ...) {
              c()
          })

#' @describeIn baseTable Return the base table of the life table
setMethod("baseTable", "valuationTable.period",
          function (object,  ...) {
              object@deathProbs
          })
