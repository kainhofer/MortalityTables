#' @include mortalityTable.period.R
NULL

#' Class mortalityTable.deMoivre - Mortality table with de Moivre's law
#'
#' A period life table with maximum age omega dn the time of death identically
#' distributed on the interval [0, omega]. The only required slot is the maximum
#' age \code{omega}, all probabilities are calculated from it.
#' Optionally, a name and loading can be passed (inherited from
#' \code{\link{mortalityTable}}).
#'
#' @slot omega      Maximum age
#'
#' @examples
#' mm = mortalityTable.deMoivre(100)
#' plot(mm,
#'      mortalityTable.deMoivre(90),
#'      mortalityTable.deMoivre(50))
#'
#' @export mortalityTable.deMoivre
#' @exportClass mortalityTable.deMoivre
mortalityTable.deMoivre = setClass(
    "mortalityTable.deMoivre",
    slots = list(
        omega = "numeric"
    ),
    prototype = list(
        omega = 100
    ),
    contains = "mortalityTable.period"
)

setMethod("initialize", "mortalityTable.deMoivre", function(.Object, omega = 100, name = NULL, ...) {
    if (missing(name) || is.null(name)) {
        name = paste("de Moivre mortality, omega=", omega)
    }
    callNextMethod(.Object, omega = omega, name = name, ages = 0:omega, deathProbs = c(1 / (omega - 0:(omega - 1)), 1), ...)
})




