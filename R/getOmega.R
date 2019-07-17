#' @include mortalityTable.R mortalityTable.period.R mortalityTable.mixed.R mortalityTable.joined.R
NULL

#' Return the maximum age of the life table
#'
#' @param object A life table object (instance of a \code{mortalityTable} class)
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' getOmega(AVOe2005R.male)
#' getOmega(mortalityTable.deMoivre(omega = 100))
#'
#' @exportMethod getOmega
setGeneric("getOmega", function(object) standardGeneric("getOmega"));

#' @describeIn getOmega Return the maximum age of the period life table
setMethod("getOmega", "mortalityTable.period",
          function(object) {
              max(object@ages, na.rm = TRUE);
          })

#' @describeIn getOmega Return the maximum age of the mixed life table
setMethod("getOmega", "mortalityTable.mixed",
          function(object) {
              getOmega(object@table1);
          })

# #' @describeIn getOmega Return the maximum age of the joined life table
# setMethod("getOmega", "mortalityTable.joined",
#           function(object) {
#               getOmega(object@table1);
#           })
