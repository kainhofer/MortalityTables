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

# browser()
    probs = table@deathProbs
    orig.probs = probs
    ages = table@ages

    if (missing(weights) || is.null(weights)) {
        if (is.na(table@exposures) || is.null(table@exposures)) {
            weights = rep(1, length(ages))
        } else {
            weights = table@exposures
        }
    }
    # Missing values are always interpolated, i.e. assigned weight 0; Similarly,
    # ignore zero probabilities (cause problems with log)
    weights = weights * (!is.na(probs) & (probs > 0))
    weights[is.na(weights)] = 0
    if (sum(probs > 0, na.rm = TRUE) < d) {
        warning("Table '", table@name, "' does not have at least ", d, " finite, non-zero probabilities. Unable to graduate. The original probabilities will be retained.")
        return(table)
    }

    # Normalize the weights to sum 1
    weights = weights / sum(weights)

    # We cannot pass NA to whittaker, as this will result in all-NA graduated values.
    # However, if prob==NA, then weight was already set to 0, anyway
    probs[is.na(probs)] = 0
    probs.smooth = exp(whittaker.interpolate(log(probs), lambda = lambda, d = d, weights = weights))

    # Do not extrapolate probabilities, so set all ages below the first and
    # above the last raw probability to NA
    probsToClear = (cumsum(!is.na(orig.probs)) == 0) | (rev(cumsum(rev(!is.na(orig.probs)))) == 0)
    probs.smooth[probsToClear] = NA_real_
    table@deathProbs = probs.smooth

    table
}


whittaker.interpolate = function(y, lambda = 1600, d = 2, weights = rep(1, length(y))) {
    m <- length(y)
    E <- eye(m)
    weights = weights * is.finite(y) # non-finite or missing values in y get zero weight
    y[!is.finite(y)] = 0
    W <- diag(weights)
    D <- diff(E, lag = 1, differences = d)# - r * diff(E, lab = 1, differences = d - 1)
    B <- W + (lambda * t(D) %*% D)
    z <- solve(B, as.vector(W %*% y))
    return(z)
}
#
# y = c(0.25288, 0, 0.31052, 0, 0, 0, 0.4062, 0, 0, 0, 0, 0.65162)
# weights = c(2115646, 0, 3413643, 0, 0, 0, 2487602, 0, 0, 0, 0, 999053)
#
#
# whittaker.interpolate(y, lambda = 10, d = 2, weights = weights)
