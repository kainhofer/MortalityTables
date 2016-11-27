#' @include mortalityTable.R mortalityTable.period.R
NULL

#' Return the base table of the life table
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param ... Other parameters (currently unused)
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' baseTable(AVOe2005R.male)
#'
#' @exportMethod baseTable
setGeneric("baseTable", function(object, ...) standardGeneric("baseTable"));

#' @describeIn baseTable Return the base table of the life table
setMethod("baseTable", "mortalityTable",
          function(object,  ...) {
              c()
          })

#' @describeIn baseTable Return the base table of the life table
setMethod("baseTable", "mortalityTable.period",
          function(object,  ...) {
              object@deathProbs
          })
