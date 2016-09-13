#' @include mortalityTable.R mortalityTable.mixed.R
NULL

#' Return the base year of the life table
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param ... Other parameters (currently unused)
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' baseYear(AVOe2005R.male)
#'
#' @exportMethod baseYear
setGeneric("baseYear", function(object, ...) standardGeneric("baseYear"));

#' @describeIn baseYear Return the base year of the life table
setMethod("baseYear", "mortalityTable",
          function (object,  ...) {
              object@baseYear
          })

#' @describeIn baseYear Return the base year of the life table
setMethod("baseYear", "mortalityTable.mixed",
          function (object,  ...) {
              baseYear(object@table1)
          })
