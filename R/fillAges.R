
#' Fill the given probabilities with NA to match the desired age range.
#'
#' @param probs Numeric vector
#' @param haveAges ages assigned to the given vector
#' @param neededAges desired age range for output
#'
#' @export  fillAges
fillAges = function(probs = c(), haveAges = c(), neededAges = NULL) {
    if (!is.null(neededAges)) {
        # initialize result with NA, then fill in all known ages from probs
        result = rep(NA_real_, length(neededAges))
        providedAges = intersect(neededAges, haveAges)
        result[match(providedAges, neededAges)] = probs[match(providedAges, haveAges)]
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
