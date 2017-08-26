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
#' @slot qxi    Death probability table of invalids (derived from mortalityTable)
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

#' @describeIn baseTable Return the base table of the joint lives mortality table (returns the base table of the first table used for joint lives)
setMethod("transitionProbabilities", "pensionTable",
          function(object, ...,  YOB = 1982) {
              x = ages(object@qx);
              q = deathProbabilities(object@qx, ..., YOB = YOB);
              i = deathProbabilities(object@ix, ..., YOB = YOB);
              data.frame(x, q, i)
          })

