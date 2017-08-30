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

#' Return all transition probabilities of the pension table
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
setGeneric("transitionProbabilities", function(object, ..., YOB = 1982) standardGeneric("transitionProbabilities"));

#' @describeIn transitionProbabilities Return all transition probabilities of the pension table
setMethod("transitionProbabilities", "pensionTable",
          function(object, ..., as.data.frame = TRUE,  YOB = 1982) {
              na.zero = function(x) { x[is.na(x)] = 0; x }
              x = ages(object@qx);
              q = na.zero(deathProbabilities(object@qx, ..., YOB = YOB));
              i = na.zero(deathProbabilities(object@ix, ..., YOB = YOB));
              qi = deathProbabilities(object@qix, ..., YOB = YOB);
              r = deathProbabilities(object@rx, ..., YOB = YOB);
              ap = deathProbabilities(object@apx, ..., YOB = YOB);
              api = deathProbabilities(object@apix, ..., YOB = YOB);
              qp = deathProbabilities(object@qpx, ..., YOB = YOB);
              h = deathProbabilities(object@hx, ..., YOB = YOB);
              qw = deathProbabilities(object@qwy, ..., YOB = YOB);
              yx = deathProbabilities(object@yx, ..., YOB = YOB);
              qg = deathProbabilities(object@qgx, ..., YOB = YOB);
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
          })


if (FALSE) {
    transitionProbabilities(AVOe2008P.male, YOB = 1977, as.data.frame = FALSE)
    epP = transitionProbabilities(EttlPagler.male, YOB = 1982)
#    avoe08p =
        transitionProbabilities(AVOe2008P.male, YOB = 1977, as.data.frame = TRUE)
}

bwRente = function(p, v) {
    Reduce(function(pp, ax1) { 1 + pp * ax1 * v }, p, 0.0, right = TRUE, accumulate = TRUE)[-(length(p) + 1)];
}


reservesThieleRecursion = function(p, ai, aij, states, i = 0.03) {
    v = 1 / (1 + i)
    res = array(0, dim = dim(ai), dimnames = dimnames(ai));
    # Recursive relation:
    #    Vi(t,A) = ai(t) + \sum_j v p_ij(t) (aij(t) + Vj(t+1,A))
    # with: ai(t) .. payment at t for being in state i
    #       aij(t) ... payment at t+1 for switching from state i to j
    #       Vi(t,A) ... reserve for payments A in state i at time t
    ThieleRecursion = function(t, Vt1) {
        rr = ai[,t] + v * rowSums(p[,,t] * aij[,,t]) + v * as.vector(p[,,t] %*% Vt1)
        as.vector(rr)
    }
    # Loop backwards over all times (starting value for reserves is 0)
    times = dimnames(p)[[3]];
    res = Reduce(f = ThieleRecursion, x = times, init = rep(0, length(states)), right = TRUE, accumulate = TRUE)[-(length(times) + 1)]
    res = do.call("cbind", res)
    dimnames(res) = dimnames(ai)
    res
}
if (FALSE) {
res = anwartschaften(AVOe2008P.female, YOB = 1977);
res
}


#' Calculates all "anwartschaften" for the gien pension table
#'
#' @param object A pension table object (instance of a \code{\linkS4class{pensionTable}} class)
#' @param ... Currently unused
#' @param i Interest rate (default 0.03)
#' @param YOB Year of birth (default 1982)
#'
#' @examples
#' pensionTables.load("Austria_*", wildcard=TRUE)
#' # anwartschaften(EttlPagler.male, i=0.03, YOB=1972)
#'
#' @exportMethod transitionProbabilities
setGeneric("anwartschaften", function(object, ...) standardGeneric("anwartschaften"));

#' @describeIn anwartschaften Calculates all "anwartschaften" for the gien pension table
setMethod("anwartschaften", "pensionTable",
          function(object, ...,  i = 0.03, YOB = 1982) {
              probs = transitionProbabilities(object, ..., YOB = YOB, as.data.frame = FALSE);

              # Time series of transition probabilities
              pp = probs$transitionProbabilities;
              x = dimnames(pp)[[3]]

              # Use a data.frame for the annuity PV with the actual ages as dimnames,
              aw = data.frame(aw = bwRente(1 - probs$widows["qw"], 1 / (1 + i)));
              dimnames(aw)[[1]] = x

              # Expected death benefit (widows)
              # Use avg. age of widow to extract the corresponding annuity present value
              # We used the age as dimname, so we can use simple subsetting
              expDeathBenefit = probs$widows[["h"]] * aw[as.character(probs$widows[["yx"]]),]

              # Build the matrix of transition payments (only on death there is
              # the widow PV as benefit, all other transitions do not yield any benefit)
              states = c("a", "i", "p", "d")
              transPayments = array(0, dim = c(4,4, length(x)), dimnames = list(states, states, x))
              transPayments["a","d",] = expDeathBenefit;
              transPayments["i","d",] = expDeathBenefit;
              transPayments["p","d",] = expDeathBenefit;

              statePayments = array(0, dim = c(4, length(x)), dimnames = list(states, x));

              aPay = reservesThieleRecursion(p = pp, ai = statePayments + c(1,0,0,0), aij = transPayments*0, states = states, i = i)
              iPay = reservesThieleRecursion(p = pp, ai = statePayments + c(0,1,0,0), aij = transPayments*0, states = states, i = i)
              pPay = reservesThieleRecursion(p = pp, ai = statePayments + c(0,0,1,0), aij = transPayments*0, states = states, i = i)
              wPay = reservesThieleRecursion(p = pp, ai = statePayments, aij = transPayments, states = states)

              list(pp = pp, transPayments = transPayments, statePayments = statePayments, aPay = aPay, iPay = iPay, pPay = pPay, wPay = wPay)
              list("a" = aPay, "i" = iPay, "p" = pPay, "w" = wPay)
});
if (FALSE) {
res = anwartschaften(AVOe2008P.female, YOB = 1977);
res

as.array(res$aPay)
str(res$aPay)


dimnames(res$pp)[[3]]

    res["102",]
res[,"aw"]
    a=15:43
a
a=array(1:8, dim=c(2,4), dimnames=list(c("a1", "a2"), c("b1", "b2", "b3", "b4"))); a
b=array(11:18, dim=c(2,4), dimnames=list(c("a1", "a2"), c("b1", "b2", "b3", "b4"))); b

array(a, b)
dimnames(a) = c(15:43)

an = anwartschaften(probs, YOB = 1977); an
showMethods("anwartschaften")
showMethods("transitionProbabilities")


array(1:12, dim = c(2,3,4), dimnames=list(c("a1", "a2"), c("b1", "b2", "b3"), c("c1", "c2", "c3", "c4")))

}
