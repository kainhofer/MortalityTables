#' Plot multiple valuation tables (life tables) in one plot
#'
#' \code{plot.valuationTable} displays multiple life tables (objects of child
#' classes of \code{valuationTable}) in one plot, with a legend showing the
#' names of the tables. If the argument \code{reference} not given, all
#' mortality rates are plotted on a log-linear scale for comparison. If the
#' argument \code{reference} is given and is a valid life table, then all
#' death probabilities are scaled by the given reference table and the y-axis
#' shows the death rates as percentage of the reference table.
#'
#' @param data First life table to be plotted. Must be a \code{valuationTable} object for the dispatcher to call this function
#' @param ... Additional life tables to be plotted (\code{valuationTable} objects)
#' @param xlim X-axis limitatation (as a two-element vector)
#' @param ylim Y-axis limitatation (as a two-element vector)
#' @param xlab X-axis label (default: "Alter")
#' @param ylab Y-axis label (default: "Sterbewahrscheinlichkeit q_x")
#' @param title The plot title
#' @param legend.position The position of the legend (default is \code{c(0.9,0.1)})
#' @param legend.key.width The keywith of the lines in the  legend (default is \code{unit(25,"mm")})
#'
#' @examples
#' # Load the Austrian census data
#' valuationTables.load("Austria_Census")
#'
#' # Plot some select census tables in a log-linear plot
#' plot(mort.AT.census.1869.male, mort.AT.census.1869.female,
#'      mort.AT.census.1971.male, mort.AT.census.1971.female,
#'      mort.AT.census.2011.male, mort.AT.census.2011.female,
#'      title="Austrian census tables",
#'      ylab=expression(q[x]), xlab="Age",
#'      xlim=c(0,90),
#'      legend.position=c(0.95,0.05))
#'
#' # Compare some census tables with the mortality of 2011 Austrian males
#' plot(mort.AT.census.1869.male, mort.AT.census.1869.female,
#'      mort.AT.census.1971.male, mort.AT.census.1971.female,
#'      mort.AT.census.2011.male, mort.AT.census.2011.female,
#'      title="Austrian Census tables, relative to 2011 males",
#'      reference=mort.AT.census.2011.male)
#'
#' @seealso \code{\link{plotValuationTables}} and \code{\link{plotValuationTableComparisons}}
#'
#' @import scales
#' @export
plot.valuationTable = function(data, ..., reference=NULL) {
    if (!missing(reference) && !is.null(reference)) {
        plotValuationTableComparisons(data, ..., reference=reference)
    } else {
        plotValuationTables(data, ...)
    }
}

