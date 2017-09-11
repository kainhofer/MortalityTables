#' @include mortalityTable.period.R
NULL

#' Class mortalityTable.MakehamGompertz - Mortality table with Makeham-Gompertz's law
#'
#' A period life table following Makeham and Gompertz's law of a mortality rate
#' \eqn{\mu} increasing exponentially with age \eqn{x} (\eqn{\mu_{x+t} = A + B c^{(x+t)}}).
#' The only required slots are the parameters \eqn{A}, \eqn{B} and \eqn{c}, all probabilities
#' are calculated from them, for technical reasons a maximum age of 120 is
#' technically assumed.  Optionally, a name and loading can be passed
#' (inherited from \code{\link{mortalityTable}}).
#'
#' @slot A          Parameter A of the Makeham-Gompertz distribution
#' @slot B          Parameter B of the Makeham-Gompertz distribution
#' @slot c          Parameter c of the Makeham-Gompertz distribution
#' @slot omega      Maximum age (default: 150)
#'
#' @examples
#' # A Gompertz mortality, roughtly approximating the Austrian annuitants 2017
#' gmp = mortalityTable.MakehamGompertz(A = 0, B = 0.00001, c = 1.11)
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#' plot(gmp, AVOe2005R.male, Period=2017)
#'
#' # A Makeham-Gompertz mortality, approximating the Austrian annuitants 2017
#' mg = mortalityTable.MakehamGompertz(A = 0.0002, B = 0.00001, c = 1.11)
#' plot(mg, gmp, AVOe2005R.male, Period=2017)
#'
#' @export mortalityTable.MakehamGompertz
#' @exportClass mortalityTable.MakehamGompertz
mortalityTable.MakehamGompertz = setClass(
    "mortalityTable.MakehamGompertz",
    slots = list(
        A = "numeric",
        B = "numeric",
        c = "numeric",
        omega = "numeric"
    ),
    prototype = list(
        A = 0,
        B = 1,
        c = 1,
        omega = 120
    ),
    contains = "mortalityTable.period"
)

setMethod("initialize", "mortalityTable.MakehamGompertz", function(.Object, A = 0, B = 1, c = 1, omega = 120, name = NULL, ...) {
    if (missing(name) || is.null(name)) {
        if (A == 0) {
            name = paste("Gompertz mortality, B=", B, ", c=", c, collapse = "")
        } else if (B == 0 || c == 1) {
            name = paste("Exponential mortality, mu=", A + B * c, collapse = "")
        } else {
            name = paste("Makeham-Gompertz mortality, A=", A, ", B=", B, ", c=", c, collapse = "")
        }
    }
    ages = 0:omega
    if (c == 1 || B == 0) {
        deathProbs = 1 - exp(-A)
    } else {
        deathProbs = 1 - exp(-A) * exp(B/log(c) * (c^ages - c^(ages + 1)))
    }
    callNextMethod(.Object, A = A, B = B, c = c, omega = omega, name = name, ages = ages, deathProbs = deathProbs, ...)
})

