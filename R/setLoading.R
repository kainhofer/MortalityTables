#' @include valuationTable.R
NULL

#' Return a copy of the table with an additional loading added
#'
#' @param object A life table object (instance of a \code{valuationTable} class)
#' @param loading The additional (security) loading to be added to the table.
#'
#' @exportMethod setLoading
setGeneric("setLoading", function(object, loading = 0) standardGeneric("setLoading"));

#' @describeIn setLoading Return the life table with the given loading set
setMethod("setLoading", "valuationTable",
          function (object, loading = 0) {
              object@loading = loading;
              object
          })
