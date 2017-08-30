#' @include pensionTable.R
NULL

bwRente = function(p, v) {
    Reduce(function(pp, ax1) { 1 + pp * ax1 * v }, p, 0.0, right = TRUE, accumulate = TRUE)[-(length(p) + 1)];
}


reservesThieleRecursion = function(p, ai, aij, states, i = 0.03) {
    v = 1 / (1 + i)

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
          function(object, ...,  i = 0.03, YOB = 1982, Period = NULL) {
              if (!is.null(Period)) {
                  probs = periodTransitionProbabilities(object, Period = Period, ..., as.data.frame = FALSE);
              } else {
                  probs = transitionProbabilities(object, YOB = YOB, ..., as.data.frame = FALSE);
              }

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

              list("a" = aPay, "i" = iPay, "p" = pPay, "w" = wPay)
          });

if (FALSE) {
    res7 = anwartschaften(AVOe2008P.female, YOB = 1977);
    res8 = anwartschaften(AVOe2008P.female, YOB = 2017);
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
