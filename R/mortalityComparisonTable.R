#' @import stats
NULL

ageRanges = function(ages, binsize = 5) {
    rangestart = floor(ages / binsize) * binsize
    sapply(X = rangestart, FUN = function(start) sprintf("%d-%d", start, start + binsize - 1) )
}

#' Calculate relative mortalities for age bands and birth years
#'
#' @param table1,table2 The \code{\link{mortalityTable}} objects to compare (mortalities of \code{table1} relative to \code{table2})
#' @param years Vector of birth years to include in the comparisons.
#' @param ages Vector of ages to include in the comparisons
#' @param binsize How many ages to combine into one age band
#' @param ... Other parameters (currently unused)
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' # Compare mortality of Austrian male and female annuitants born 1930 to 2030
#' mortalityComparisonTable(
#'     AVOe2005R.male, AVOe2005R.female,
#'     years = seq(1930, 2030, by = 10),
#'     ages = 0:119)
#'
#' # Compare the two Austrian male annuity tables AVOe 2005-R and AVOe 1996-R,
#' # combining ages 10-19, 20-29, etc.
#' mortalityComparisonTable(
#'     AVOe2005R.male, AVOe1996R.male,
#'     years = seq(1930, 2030, by = 10),
#'     ages = 0:109, binsize=10)
#'
#'
#' @export
mortalityComparisonTable = function(table1, table2, years, ages, binsize = 5, ...) {
    q1 = as.data.frame(
        sapply(years, FUN = function(y) { deathProbabilities(table1, YOB = y) }),
        row.names = ages(table1)
    );
    colnames(q1) <- years
    # Select only the given ages!
    q1 = q1[as.character(ages),]

    q2 = as.data.frame(
        sapply(years, FUN = function(y) { deathProbabilities(table2, YOB = y) }),
        row.names = ages(table2)
    );
    colnames(q2) <- years
    # Select only the given ages!
    q2 = q2[as.character(ages),]


    # Calculate the ratios of female:male mortality and average in bin sizes of 5:
    ratios = (q1/q2)
    ageRanges = ageRanges(as.numeric(rownames(ratios)), binsize=binsize)
    averages = aggregate(ratios, by = list(Ages = factor(ageRanges, levels = unique(ageRanges))), FUN = mean)
    rownames(averages) = averages$Ages
    averages$Ages = NULL

    averages
}

# mortalityComparisonTable(AVOe2008P.female.aa, AVOe2008P.male.aa, years = seq(1932, 2022, by = 10), ages = 15:119, binsize=10)
