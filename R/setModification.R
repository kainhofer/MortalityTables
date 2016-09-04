#' @include mortalityTable.R
NULL

#' Return a copy of the table with the given modification function added
#'
#' @param object A life table object (instance of a \code{mortalityTable} class)
#' @param modification The postprocessing modification function (for example, so enforce a lower bound).
#'
#' @exportMethod setModification
setGeneric("setModification", function(object, modification = 0) standardGeneric("setModification"));

#' @describeIn setModification Return the life table with the given modification set
setMethod("setModification", "mortalityTable",
          function (object, modification = 0) {
              object@modification = modification;
              object
          })
