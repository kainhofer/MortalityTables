#' @include mortalityTable.R
NULL

#' Return the cohort life table as a \code{mortalityTable.period} object
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param YOB The birth year for which the life table should be calculated
#' @param ... Other parameters (currently unused)
#'
#' @exportMethod getCohortTable
setGeneric("getCohortTable", function(object, YOB, ...) standardGeneric("getCohortTable"));

#' @describeIn getCohortTable Return the cohort life table as a
#'             \code{mortalityTable.period} object
setMethod("getCohortTable","mortalityTable",
          function(object, YOB, ...) {
              mortalityTable.period(
                  name = paste(object@name, ", YOB ", YOB),
                  baseYear = YOB,
                  ages = ages(object),
                  deathProbs = deathProbabilities(object, YOB = YOB, ...)
              );
          })
