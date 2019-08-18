#' Smooth a life table using the Whittaker-Henderson method, intepolation possibly missing values
#'
#' \code{whittaker.mortalityTable} uses the Whittaker-Henderson graduation method
#' to smooth a table of raw observed death probabilities, optionally using the
#' exposures stored in the table as weights (if no exposures are given, equal
#' weights are applied). The weights (either explicitly given, implicitly taken
#' from the exposures or implicit equal weights) will be normalized to sum 1.
#' The parameter lambda indicates the importance of smootheness. A lower value of lambda
#' will put more emphasis on reproducing the observation as good as possible at the cost of
#' less smoothness. In turn, a higher value of lambda will force the smoothed result to be
#' as smooth as possible with possibly larger deviation from the input data.
#' All ages with a death probability of \code{NA} will be
#' interpolated in the Whittaker-Henderson method (see e.g. Lowrie)
#'
#' @param table Mortality table to be graduated. Must be an instance of a
#'              \code{mortalityTable}-derived class.
#' @param lambda Smoothing parameter (default 10)
#' @param d order of differences (default 2)
#' @param name.postfix Postfix appended to the name of the graduated table
#' @param weights Vector of weights used for graduation. Entries with weight 0
#'                will be interpolated. If not given, the exposures of the table
#'                or equal weights are used. Weight 0 for a certain age indicates
#'                that the observation will not be used for smoothing at all,
#'                and will rather be interpolated from the smoothing of all other values.
#' @param log Whether the smoothing should be applied to the logarithms of the
#'            table values or the values itself
#' @param ... additional arguments (currently unused)
#'
#' @references
#' Walter B. Lowrie: An Extension of the Whittaker-Henderson Method of Graduation, Transactions of Society of Actuaries, 1982, Vol. 34, pp. 329--372
#'
#' @examples
#' # A sample observation table with exposures and raw probabilities
#' obsTable = mortalityTable.period(
#'     name = "trivial observed table",
#'     ages = 0:15,
#'     deathProbs = c(
#'         0.0072, 0.00212, 0.00081, 0.0005, 0.0013,
#'         0.001, 0.00122, 0.00142, 0.007, 0.0043,
#'         0.0058, 0.0067, 0.0082, 0.0091, 0.0075, 0.01),
#'     exposures = c(
#'         150, 222, 350, 362, 542,
#'         682, 1022, 1053, 1103, 1037,
#'         968, 736, 822, 701, 653, 438))
#'
#' # Effect of the different parameters
#' obsTable.smooth = whittaker.mortalityTable(obsTable,
#'     lambda = 1/10, d = 2, name.postfix = " smoothed (d=2, lambda=1/10)")
#' obsTable.smooth1 = whittaker.mortalityTable(obsTable,
#'     lambda = 1, d = 2, name.postfix = " smoothed (d=2, lambda=1)")
#' obsTable.smooth2 = whittaker.mortalityTable(obsTable,
#'     lambda = 1/10, d = 3, name.postfix = " smoothed (d=3, lambda=1/10)")
#' plot(obsTable, obsTable.smooth, obsTable.smooth1, obsTable.smooth2,
#'     title = "Observed death probabilities")
#'
#' # Missing values are interpolated from the Whittaker Henderson
#' obsTable.missing = obsTable
#' obsTable.missing@deathProbs[c(6,10,11,12)] = NA_real_
#' obsTable.interpolated = whittaker.mortalityTable(obsTable,
#'     lambda = 1/10, d = 2, name.postfix = " missing values interpolated")
#' plot(obsTable.missing, obsTable.interpolated,
#'     title = "Missing values are automatically interpolated") + geom_point(size = 3)
#'
#'
#' @seealso \code{\link[pracma]{whittaker}}
#'
#' @import scales
#' @export
whittaker.mortalityTable = function(table, lambda = 10, d = 2, name.postfix = ", smoothed", ..., weights = NULL, log = TRUE) {
    if (is.array(table)) {
        return(array(
            lapply(table, whittaker.mortalityTable, lambda = lambda, d = d, name.postfix = name.postfix, ..., weights = weights, log = log),
            dim = dim(table), dimnames = dimnames(table))
        )
    } else if (is.list(table)) {
        return(lapply(table, whittaker.mortalityTable, lambda = lambda, d = d, name.postfix = name.postfix, ..., weights = weights, log = log))
    } else if (is.na(c(table))) {
        return(table)
    }
    if (!is(table, "mortalityTable")) {
        stop("Table object must be an instance (or list of instances) of mortalityTable in whittaker.mortalityTable.")
    }
    # append the postfix to the table name to distinguish it from the original (raw) table
    if (!is.null(name.postfix)) {
        table@name = paste0(table@name, name.postfix)
    }

    probs = table@deathProbs
    orig.probs = probs
    ages = table@ages

    if (missing(weights) || is.null(weights)) {
        if (is.null(table@exposures) || is.na(table@exposures)) {
            weights = rep(1, length(ages))
        } else {
            weights = table@exposures
        }
    }
    # Missing values are always interpolated, i.e. assigned weight 0;
    weights = weights * !is.na(probs)
    # Similarly, for log-smoothing ignore zero probabilities (cause problems with log)
    if (log) {
        weights = weights * (probs > 0)
    }
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
    if (log) {
        probs.smooth = exp(whittaker.interpolate(log(probs), lambda = lambda, d = d, weights = weights))
    } else {
        probs.smooth = whittaker.interpolate(probs, lambda = lambda, d = d, weights = weights)
    }

    # Do not extrapolate probabilities, so set all ages below the first and
    # above the last raw probability to NA
    probsToClear = (cumsum(!is.na(orig.probs)) == 0) | (rev(cumsum(rev(!is.na(orig.probs)))) == 0)
    probs.smooth[probsToClear] = NA_real_
    table@data$rawProbs = orig.probs
    table@data$whittaker = list(weights = weights)
    table@deathProbs = probs.smooth

    table
}


whittaker.interpolate = function(y, lambda = 1600, d = 2, weights = rep(1, length(y))) {
    m <- length(y)
    E <- diag(1, m, m)
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
