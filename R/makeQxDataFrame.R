#' Converts one or multiple mortality table objects to a data frame that can be
#' plotted by \code{plotMortalityTables} or \code{plotMortalityTableComparisons}
#'
#' It is not required to call this function manually, \code{plotMortalityTables}
#' will automatically do it if object derived from class \code{mortalityTable}
#' are passed.
#'
#' @param ... Life tables (objects of classes derived from \code{mortalityTable})
#' @param YOB desired year of birth to be plotted as cohort life table (default: 1972)
#' @param Period desired observation year to be plotted (default: NA). If both
#'        \code{YOB} and \code{Period} are given, a period comparison is generated.
#' @param reference Reference life table, used to show relative death
#'        probabilities (i.e. the q_x for all ages are divided by the
#'        corresponding probabilities of the reference table)
#'
#' @export
makeQxDataFrame = function(..., YOB = 1972, Period = NA, reference = NULL) {
    # If reference is given, normalize all probabilities by that table!
    data = list(...);
    names(data) = lapply(data, function(t) t@name);
    reference_ages = NULL;

    if (missing(Period)) {
        if (!missing(reference)) {
            reference_ages = ages(reference);
            reference = deathProbabilities(reference, YOB = YOB);
        }
        data = lapply(data, function(t) {
            normalize_deathProbabilities(
                cbind(x = ages(t), y = deathProbabilities(t, YOB = YOB)),
                reference = reference,
                referenceAges = reference_ages)
        });
    } else {
        if (!missing(reference)) {
            reference_ages = ages(reference);
            reference = periodDeathProbabilities(reference, Period = Period);
        }
        data = lapply(data, function(t) {
            normalize_deathProbabilities(
                cbind(x = ages(t), y = periodDeathProbabilities(t, Period = Period)),
                reference = reference,
                referenceAges = reference_ages)
        });
    }

    list.names = names(data)
    lns <- sapply(data, nrow)
    data <- as.data.frame(do.call("rbind", data))
    data$group <- rep(list.names, lns)
    data
}

normalize_deathProbabilities = function (data, reference = NULL, referenceAges = NULL) {
    if (missing(reference) || missing(referenceAges) || is.null(reference) || is.null(referenceAges)) {
        return(data);
    }
    # Find which ages exist in both and obtain those indices from the data and the reference list:
    useages = intersect(data[,"x"], referenceAges)
    dataindices = match(useages, data[,"x"])
    refindices = match(useages, referenceAges)

    # Find which ages in data do NOT exist in the reference ages (and are thus NOT normalized at all)
    # Print a warning!
    missingrefs = setdiff(data[,"x"], referenceAges)
    if (length(missingrefs)>0) {
        warning("Reference mortality table does not contain ages ",
                missingrefs,
                " required for normalization. These ages will not be normalized!")
    }

    # Now divide the data by the corresponding entries from the reference list
    data[dataindices, "y"] = data[dataindices, "y"] / reference[refindices]
    data
}

