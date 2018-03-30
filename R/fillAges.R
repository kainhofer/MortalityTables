
#' Fill the given probabilities with NA to match the desired age range.
#'
#' @param probs Numeric vector
#' @param givenAges ages assigned to the given vector
#' @param neededAges desired age range for output
#' @param fill 	If set, missing values will be replaced with this value. Default is to fill with NA.
#'
#' @examples
#' # Ages 20-70 have linearly increasing death probabilities. Fill with 0 for the whole age range 0-120
#' fillAges(probs = c(0:50/50), givenAges = 20:70, neededAges = 0:120, fill = 0)
#'
#' @export  fillAges
fillAges = function(probs = c(), givenAges = c(), neededAges = NULL, fill = NA_real_) {
    if (!is.null(neededAges)) {
        # initialize result with NA, then fill in all known ages from probs
        result = rep(fill, length(neededAges))
        providedAges = intersect(neededAges, givenAges)
        result[match(providedAges, neededAges)] = probs[match(providedAges, givenAges)]
        result
    } else {
        probs
    }
}
#
# haveAges = c(12,16,20, 23:30, 32, 40)
# neededAges = c(0:24, 49:25)
# probs = c(12,16,20, 23:30, 32, 40)/10
#
# providedAges = intersect(neededAges, haveAges)
# result = rep(NA_real_, length(neededAges))
#
#
#
#
# names(result) = neededAges
# result
