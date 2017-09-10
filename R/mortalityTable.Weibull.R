#' @include mortalityTable.period.R
NULL

#' Class mortalityTable.Weibull - Mortality table with Weibull's law
#'
#' A period life table following Weibulls's law of a mortality rate
#' $\mu$ increasing as a power of $t$ ($\mu_{x+t} = k * (x+t)^n$).
#' The only required slots are the parameters $k>0$ and $n>0$, all probabilities
#' are calculated from them, for technical reasons a maximum age of 150 is
#' technically assumed.  Optionally, a name and loading can be passed
#' (inherited from \code{\link{mortalityTable}}).
#'
#' @slot k          Parameter k of the Weibull distribution
#' @slot n          Parameter n of the Weibull distribution
#' @slot omega      Maximum age (default: 120)
#'
#' @examples
#' # A Weibull mortality
#' wbl = mortalityTable.Weibull(k = 0.0000000001, n = 4.8)
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#' plot(wbl, AVOe2005R.male, Period=2017, ylim = c(0.00005, 1))
#'
#' @export mortalityTable.Weibull
#' @exportClass mortalityTable.Weibull
mortalityTable.Weibull = setClass(
    "mortalityTable.Weibull",
    slots = list(
        k = "numeric",
        n = "numeric",
        omega = "numeric"
    ),
    prototype = list(
        k = 0.005,
        n = 5,
        omega = 120
    ),
    contains = "mortalityTable.period"
)

setMethod("initialize", "mortalityTable.Weibull", function(.Object, k = 1, n = 1, omega = 120, name = NULL, ...) {
    if (missing(name) || is.null(name)) {
        name = paste("Weibull mortality, k=", k, ", n=", n, collapse = "")
    }
    ages = 0:omega
    deathProbs = 1 - exp(-k / (n + 1) * ((ages + 1)^(n + 1) - ages^(n + 1)))
    callNextMethod(.Object, k = k, n = n, omega = omega, name = name, ages = ages, deathProbs = deathProbs, ...)
})

