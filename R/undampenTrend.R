#' @include valuationTable.trendProjection.R
NULL

#' Return a \code{valuationTable.trendProjection} object with the trend damping removed.
#'
#' @param object The life table object (class inherited from valuationTable)
#'
#' @exportMethod undampenTrend
setGeneric("undampenTrend", function (object) standardGeneric("undampenTrend"));

#' @describeIn undampenTrend Return a \code{valuationTable.trendProjection}
#'             object with the trend damping removed.
setMethod("undampenTrend", "valuationTable.trendProjection",
          function (object) {
              object@dampingFunction=identity;
              object
          })
