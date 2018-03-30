#' @include mortalityTable.R
NULL

#' Return a copy of the table with an additional loading added
#'
#' @param object A life table object (instance of a \code{mortalityTable} class)
#' @param loading The additional (security) loading to be added to the table.
#'
#' @examples
#' mortalityTables.load("Austria_Census")
#' # Austrian census mortality 2011 reduced by 30%
#' setLoading(mort.AT.census.2011.male, loading = -0.3)
#'
#' @exportMethod setLoading
setGeneric("setLoading", function(object, loading = 0) standardGeneric("setLoading"));

#' @describeIn setLoading Return the life table with the given loading set
setMethod("setLoading", "mortalityTable",
          function (object, loading = 0) {
              object@loading = loading;
              object
          })
