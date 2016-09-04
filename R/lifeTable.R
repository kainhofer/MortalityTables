#' @include mortalityTable.R
NULL

#' Return the lifetable object (package lifecontingencies) for the cohort life table
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param ... Parameters to be passed to the \code{deathProbabilities} method
#'            of the life table
#'
#' @exportMethod lifeTable
setGeneric("lifeTable", function(object, ...) standardGeneric("lifeTable"));

#' @describeIn lifeTable Return the lifetable object (package lifecontingencies)
#'             for the cohort life table
setMethod("lifeTable","mortalityTable",
          function (object,  ...) {
              qx = deathProbabilities(object, ...);
              if (qx[[length(qx)]] != 1) {
                  qx = c(qx, 1, 1);
              }
              lifecontingencies::probs2lifetable(qx, type = "qx")
          })
