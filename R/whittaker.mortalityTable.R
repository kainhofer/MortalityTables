#' Smooth a life table using the Whittaker-Henderson method, intepolation possibly missing values
#'
#' \code{whittaker.mortalityTable} uses the Whittaker-Henderson graduation method
#' to smooth a table of raw observed death probabilities, optionally using the
#' exposures stored in the table as weights (if no exposures are given, equal
#' weights are applied). All ages with a death probability of \code{NA} will be
#' interpolated in the Whittaker-Henderson method (see e.g. Lowrie)
#'
#' @param table Mortality table to be graduated. Must be an instance of a
#'              \code{mortalityTable}-derived class.
#' @param lambda Smoothing parameter (default 10)
#' @param d order of differences (default 2)
#' @param name.postfix Postfix appended to the name of the graduated table
#' @param weights Vector of weights used for graduation. Entries with weight 0
#'                will be interpolated. If not given, the exposures of the table
#'                or equal weights are used.
#'
#' @param ... additional arguments (currently unused)
#' @param reference The reference table that determines the 100\% values.
#'                  If not given, the absolute mortality values are
#'                  compared and plotted on a log-linear scale.
#' @param trend If set to \code{TRUE}, the function \code{\link{plotMortalityTrend}}
#'              is used to plot the trends of the given tables.
#'
#' @references
#' Walter B. Lowrie: An Extension of the Whittaker-Henderson Method of Graduation, Transactions of Society of Actuaries, 1982, Vol. 34, pp. 329--372
#'
#' @examples
#' # TODO
#'
#' @seealso \code{\link{whittaker}}
#'
#' @import scales
#' @export
whittaker.mortalityTable = function(table, lambda = 10, d = 2, name.postfix = ", smoothed", ..., weights = NULL) {
    if (!is(table, "mortalityTable")) {
        stop("Table object must be an instance of mortalityTable in whittaker.mortalityTable.")
    }
    # append the postfix to the table name to distinguish it from the original (raw) table
    if (!is.null(name.postfix)) {
        table@name = paste0(table@name, name.postfix)
    }

    probs = table@deathProbs
    ages = table@ages

    if (missing(weights) || is.null(weights)) {
        if (is.na(table@exposures) || is.null(table@exposures)) {
            weights = table@exposures
        } else {
            weights = rep(1, length(ages))
        }
    }
    # Missing values are always interpolated, i.e. assigned weight 0:
    weights = weights * is.na(probs)

    probs.smooth = exp(whittaker.interpolate(log(probs[!NApos]), lambda = lambda, d = d, weights = weights))
    table@deathProbs = probs.smooth

    table
}


whittaker.interpolate = function(y, lambda = 1600, d = 2, weights = rep(1, length(y))) {
    m <- length(y)
    E <- eye(m)
    W <- diag(weights)
    D <- diff(E, lag = 1, differences = d)# - r * diff(E, lab = 1, differences = d - 1)
    B <- W + (lambda * t(D) %*% D)
    z <- solve(B, W %*% y)
    return(z)
}
#
# y = c(0.25288, 0, 0.31052, 0, 0, 0, 0.4062, 0, 0, 0, 0, 0.65162)
# weights = c(2115646, 0, 3413643, 0, 0, 0, 2487602, 0, 0, 0, 0, 999053)
#
#
# whittaker.interpolate(y, lambda = 10, d = 2, weights = weights)
