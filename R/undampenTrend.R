#' @include mortalityTable.trendProjection.R
NULL

#' Return a \code{mortalityTable.trendProjection} object with the trend damping removed.
#'
#' @param object The life table object (class inherited from mortalityTable)
#'
#' @exportMethod undampenTrend
setGeneric("undampenTrend", function (object) standardGeneric("undampenTrend"));

#' @describeIn undampenTrend Return a \code{mortalityTable.trendProjection}
#'             object with the trend damping removed.
setMethod("undampenTrend", "mortalityTable.trendProjection",
          function (object) {
              object@dampingFunction=identity;
              object
          })
