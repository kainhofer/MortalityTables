#' @include mortalityTable.R
NULL


#' Class pensionTable
#'
#' Class \code{pensionTable} is the (virtual) base class for all pensions
#' tables. It contains the name and some general values applying to all
#' types of tables. In particular, it holds individual tables for each of the
#' transition probabilities. Possible states are:
#'     * active: healty, no pension, typically paying some kin of premium
#'     * incapacity: disablity pension, in most cases permanent, not working, early pension
#'     * retirement: old age pension, usually starting with a fixed age
#'     * dead
#'       * Widow/widower pension
#'
#' Correspondingly, the following transition probabilities can be given:
#'     * qxaa:  death probability of actives (active -> dead)
#'     * ix:    invalidity probability (active -> incapacity)
#'     * qix:   death probability of invalid (invalid -> dead)
#'     * rx:    reactivation probability (incapacity -> active)
#'     * apx:   retirement probability (active -> retirement), typically 1 for a fixed age
#'     * apx:   retirement probability of invalids (invalid -> retirement), typically 0 or 1 for a fixed age
#'     * qpx:   death probability of retired (retired -> dead)
#'     * hx:    probability of a widow at moment of death (dead -> widow), y(x) age differene
#'     * qxw:   death probability of widows/widowers
#'     * qgx:   death probability of total group (irrespective of state)
#'
#' @slot qx     Death probability table of actives (derived from mortalityTable)
#' @slot ix     Invalidity probability of actives (derived from mortalityTable)
#' @slot qix    Death probability table of invalids (derived from mortalityTable)
#' @slot rx     Reactivation probability of invalids (derived from mortalityTable)
#' @slot apx    Retirement probability of actives (derived from mortalityTable)
#' @slot apix   Retirement probability of invalids (derived from mortalityTable)
#' @slot qpx    Death probability of old age pensioners (derived from mortalityTable)
#' @slot hx     Probability of a widow at the moment of death (derived from mortalityTable)
#' @slot qwy    Death probability of widow(er)s (derived from mortality Table)
#' @slot yx     Age difference of the widow to the deceased
#' @slot qgx    Death probability of whole group (derived from mortalityTable), irrespective of state
#'
#' @export pensionTable
#' @exportClass pensionTable
pensionTable = setClass(
    "pensionTable",
    slots = list(
        qx    = "mortalityTable",
        ix    = "mortalityTable",
        qix   = "mortalityTable",
        rx    = "mortalityTable",
        apx   = "mortalityTable",
        apix  = "mortalityTable",
        qpx   = "mortalityTable",
        hx    = "mortalityTable",
        qwy   = "mortalityTable",
        yx    = "mortalityTable",
        qgx   = "mortalityTable"
    ),
    contains = "mortalityTable"
)

pensionTableProbArrange = function(x, q, i, qi, r, ap, api, qp, h, qw, yx, qg, as.data.frame = TRUE) {
    if (as.data.frame) {
        data.frame(x, q, i, qi, r, ap, api, qp, h, qw, yx, qg)
    } else {
        states = c("a", "i", "p", "d")
        transProb = array(0, dim = c(4,4, length(x)), dimnames = list(states, states, x))

        transProb["a", "a", ] = (1 - i - q) * (1 - ap);
        transProb["a", "i", ] = i;
        transProb["a", "p", ] = (1 - q - i ) * ap;
        transProb["a", "d", ] = q;

        transProb["i", "a", ] = r;
        transProb["i", "i", ] = (1 - qi - r) * (1 - api);
        transProb["i", "p", ] = (1 - qi - r) * api;
        transProb["i", "d", ] = qi;

        transProb["p", "p", ] = 1 - qp;
        transProb["p", "d", ] = qp;

        transProb["d", "d", ] = 1;

        list(transitionProbabilities = transProb, widows = data.frame(x, h, qw, yx))
    }
}

#' Return all transition probabilities of the pension table (generational probabilities)
#'
#' @param object A pension table object (instance of a \code{\linkS4class{pensionTable}} class)
#' @param ... Currently unused
#' @param YOB Year of birth
#'
#' @examples
#' pensionTables.load("Austria_*", wildcard=TRUE)
#' # transitionProbabilities(EttlPagler.male)
#'
#' @exportMethod transitionProbabilities
setGeneric("transitionProbabilities", function(object, ...) standardGeneric("transitionProbabilities"));

#' @describeIn transitionProbabilities Return all transition probabilities of the pension table for the generation YOB
setMethod("transitionProbabilities", "pensionTable",
          function(object, YOB = 1982, ..., as.data.frame = TRUE) {
              na.zero = function(x) { x[is.na(x)] = 0; x }
              x   = ages(object@qx);
              q   = na.zero(deathProbabilities(object@qx, ..., YOB = YOB));
              i   = na.zero(deathProbabilities(object@ix, ..., YOB = YOB));
              qi  = deathProbabilities(object@qix, ..., YOB = YOB);
              r   = deathProbabilities(object@rx, ..., YOB = YOB);
              ap  = deathProbabilities(object@apx, ..., YOB = YOB);
              api = deathProbabilities(object@apix, ..., YOB = YOB);
              qp  = deathProbabilities(object@qpx, ..., YOB = YOB);
              h   = deathProbabilities(object@hx, ..., YOB = YOB);
              qw  = deathProbabilities(object@qwy, ..., YOB = YOB);
              yx  = deathProbabilities(object@yx, ..., YOB = YOB);
              qg  = deathProbabilities(object@qgx, ..., YOB = YOB);
              pensionTableProbArrange(x, q, i, qi, r, ap, api, qp, h, qw, yx, qg, as.data.frame = as.data.frame)
          })

#' Return all period transition probabilities of the pension table
#'
#' @param object A pension table object (instance of a \code{\linkS4class{pensionTable}} class)
#' @param Period Observation year
#' @param ... Currently unused
#' @param as.data.frame Whether the return value should be a data.frame or an array containing transition matrices
#'
#' @examples
#' pensionTables.load("Austria_*", wildcard=TRUE)
#' # periodTransitionProbabilities(EttlPagler.male, Period = 2017)
#'
#' @exportMethod periodTransitionProbabilities
setGeneric("periodTransitionProbabilities", function(object, ...) standardGeneric("periodTransitionProbabilities"));


#' @describeIn periodTransitionProbabilities Return all transition probabilities of the pension table for the period Period
setMethod("periodTransitionProbabilities", "pensionTable",
          function(object, Period = 2017, ..., as.data.frame = TRUE) {
              na.zero = function(x) { x[is.na(x)] = 0; x }
              x   = ages(object@qx);
              q   = na.zero(periodDeathProbabilities(object@qx, ..., Period = Period));
              i   = na.zero(periodDeathProbabilities(object@ix, ..., Period = Period));
              qi  = periodDeathProbabilities(object@qix, ..., Period = Period);
              r   = periodDeathProbabilities(object@rx, ..., Period = Period);
              ap  = periodDeathProbabilities(object@apx, ..., Period = Period);
              api = periodDeathProbabilities(object@apix, ..., Period = Period);
              qp  = periodDeathProbabilities(object@qpx, ..., Period = Period);
              h   = periodDeathProbabilities(object@hx, ..., Period = Period);
              qw  = periodDeathProbabilities(object@qwy, ..., Period = Period);
              yx  = periodDeathProbabilities(object@yx, ..., Period = Period);
              qg  = periodDeathProbabilities(object@qgx, ..., Period = Period);
              pensionTableProbArrange(x, q, i, qi, r, ap, api, qp, h, qw, yx, qg, as.data.frame = as.data.frame)
          })


if (FALSE) {
    transitionProbabilities(AVOe2008P.male, YOB = 1977, as.data.frame = FALSE)
    epP = transitionProbabilities(EttlPagler.male, YOB = 1982)
#    avoe08p =
        transitionProbabilities(AVOe2008P.male, YOB = 1977, as.data.frame = TRUE)
}
