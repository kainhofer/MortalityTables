#' @include mortalityTable.R periodDeathProbabilities.R
NULL

setClassUnion("mortalityTable(s)", c("mortalityTable", "list"))
#' Class mortalityTable.jointLives - Life table for multiple joint lives
#'
#' A cohort life table obtained by calculating joint death probabilities for
#' multiple lives, each possibly using a different mortality table.
#'
#' @slot table The \code{mortalityTable} object for all lives (vector if different tables should be used for the different persons)
#'
#' @examples
#' mortalityTables.load("Germany_Census")
#' table.JL = mortalityTable.jointLives(
#'     name = "ADSt 24/26 auf verbundene Leben",
#'     table = mort.DE.census.1924.26.male
#' )
#' deathProbabilities(table.JL, YOB = 1977, ageDifferences = c(1, 5, -5, 16))
#' deathProbabilities(table.JL, YOB = 1977, ageDifferences = c(0))
#' deathProbabilities(table.JL, YOB = 1977, ageDifferences = c(1, 5, 16))
#'
#' @export mortalityTable.jointLives
#' @exportClass mortalityTable.jointLives
mortalityTable.jointLives = setClass(
    "mortalityTable.jointLives",
    slots = list(
        table = "mortalityTable(s)"
    ),
    contains = "mortalityTable"
)


pad0 = function(v, l, value=0) {
    if (l >= length(v)) {
        c(v, rep(value, l - length(v)))
    } else {
        v[0:l]
    }
}
padLast = function(v, l) {
    pad0(v, l, utils::tail(v, n = 1))
}

#' Return a matrix of the persons' individual death probabilities of a joint-life
#' table (instance of \code{\link{mortalityTable.jointLives}})
#'
#' The rows will always be the full age range of the first life, i.e. the first
#' row describes the mortality of the joint-life state when the first person is
#' in its first year of life. The last row will be for the maximum age of the first
#' life, even if other lives have shorter tables. These will be padded with the
#' last available value (which for life tables should be q_x=1 anyway).
#'
#' @param tables List of life table objects (object inherited from
#'               \code{\link{mortalityTable}})
#' @param YOB The birth year for the first person
#' @param ageDifferences The age differences to the first person
#'
#' @examples
#' mortalityTables.load("Germany_Census")
#' deathProbabilitiesIndividual(list(mort.DE.census.1924.26.male), 1977, c(0, 0))
#' deathProbabilitiesIndividual(list(mort.DE.census.1924.26.male), 1977, c(0, -5, 13))
#'
#' @export deathProbabilitiesIndividual
deathProbabilitiesIndividual = function(tables, YOB = NULL, ageDifferences = NULL) {
    n = max(length(YOB), length(ageDifferences) + 1);
    if (length(YOB) == 1) {
        YOB = c(YOB, YOB + ageDifferences);
    }
    if (length(ageDifferences) < length(YOB) - 1) {
        ageDifferences = diff(YOB);
    }
    # prepend a 0, because the first entry has no offset
    ageDifferences = c(0, ageDifferences);
    tables = padLast(tables, n);

    # Find the required length to have all (shifted) death probabilities fit
    # last value will be repeated for shorter tables
    # FIXME: For now, wee cannot make the table longer than the first table, because
    # ages(...) will always just return a list of ages allowed for the first table.
    # The reason is that the deathProbabilities function gets a list of ageDifferences
    # influencing the possible length of the death probabilities, while the ages
    # function has only the mortalityTable.2Lives object without any further information,
    # i.e. the age differences are not part of the mortality table definition,
    # but ages(...) has only access to that definition and nothing else.
    # qxlen = max(mapply(
    #     function(table, yob, difference) {
    #         getOmega(table) - difference
    #     },
    #     tables, YOB, ageDifferences)) + 1;
    qxlen = getOmega(tables[[1]]) + 1;
    qxMatrix = mapply(
        function(table, yob, difference) {
            qx = deathProbabilities(table, yob);
            if (difference <= 0) {
                # Person is younger, so we need to pad with qx=0 for x<=difference, i.e. pad with difference zeroes
                # This code also works with difference==0!
                qxtmp = c(
                    rep(0, -difference),
                    qx);
            } else {
                qxtmp = utils::tail(qx, -difference);
            }
            qxnew = padLast(qxtmp, qxlen)
            qxnew
        },
        tables, YOB, ageDifferences);
    qxMatrix
}

#' Return a matrix of the persons' individual period death probabilities of a
#' joint-life table (instance of \code{\link{mortalityTable.jointLives}})
#'
#' @param tables List of life table objects (object inherited from
#'               \code{\link{mortalityTable}})
#' @param period The observation period
#' @param ageDifferences The age differences to the first person
#'
#' @examples
#' mortalityTables.load("Germany_Census")
#' periodDeathProbabilitiesIndividual(list(mort.DE.census.1924.26.male), 1977, c(0, 0))
#' periodDeathProbabilitiesIndividual(list(mort.DE.census.1924.26.male), 1977, c(0, -5, 13))
#'
#' @export periodDeathProbabilitiesIndividual
periodDeathProbabilitiesIndividual = function(tables, period, ageDifferences) {
    # prepend a 0, because the first entry has no offset
    ageDifferences = c(0, ageDifferences);
    tables = padLast(tables, length(ageDifferences));

    # Find the required length to have all (shifted) death probabilities fit
    # last value will be repeated for shorter tables
    # FIXME: For now, wee cannot make the table longer than the first table, because
    # ages(...) will always just return a list of ages allowed for the first table.
    # The reason is that the deathProbabilities function gets a list of ageDifferences
    # influencing the possible length of the death probabilities, while the ages
    # function has only the mortalityTable.2Lives object without any further information,
    # i.e. the age differences are not part of the mortality table definition,
    # but ages(...) has only access to that definition and nothing else.
    # qxlen = max(mapply(
    #     function(table, yob, difference) {
    #         getOmega(table) - difference
    #     },
    #     tables, YOB, ageDifferences)) + 1;
    qxlen = getOmega(tables[[1]]) + 1;
    qxMatrix = mapply(
        function(table, difference) {
            qx = periodDeathProbabilities(table, Period = period);
            if (difference <= 0) {
                # Person is younger, so we need to pad with qx=0 for x<=difference, i.e. pad with difference zeroes
                # This code also works with difference==0!
                qxtmp = c(
                    rep(0, -difference),
                    qx);
            } else {
                qxtmp = utils::tail(qx, -difference);
            }
            qxnew = padLast(qxtmp, qxlen)
            qxnew
        },
        tables, ageDifferences);
    qxMatrix
}

#' @describeIn ages Return the defined ages of the joint lives mortality table (returns the ages of the first table used for joint lives)
setMethod("ages", "mortalityTable.jointLives",
          function(object, ...) {
              ages(c(object@table)[[1]], ...);
          })

#' @describeIn baseTable Return the base table of the joint lives mortality table (returns the base table of the first table used for joint lives)
setMethod("baseTable", "mortalityTable.jointLives",
          function(object,  ...) {
              baseTable(c(object@table)[[1]], ...)
          })

#' @describeIn baseYear Return the base year of the life table
setMethod("baseYear", "mortalityTable.jointLives",
          function(object,  ...) {
              baseYear(c(object@table)[[1]], ...)
          })

normalizeJointAges = function(YOB = NULL, ages = NULL, ageDifferences = NULL, agesJoint = NULL) {
    # The ages and years of birth of the joint lives can be given in multiple arguments. They will be evaluated in the following order:
    #   * YOB: vector of birth years for all joint lives
    #   * YOB (scalar, for the first life) and ageDifferences: relative to the first life
    #   * YOB (scalar, for the first life) and agesJoint (explicit list of all ages of the joint lives)
    #   * ages (vector of requested ages, for the first life ONLY) and ageDifferences: relative to the first life
    #   * agesJoint: numeric vector of individual ages
    #   * ageDifferences only (not permitted for cohort life tables)

    is_scalar <- function(x) !is.null(x) && length(x) == 1L && !is.na(x)
    is_vector <- function(x) !is.null(x) && length(x) >= 1L

    # CASE 1) YOB vector for all joint lives
    if (is_vector(YOB) && length(YOB) > 1L) {
        YOBv = as.numeric(YOB)
        return(list(YOB = YOBv, ageDifferences = YOBv[1] - YOBv[-1]))
    }

    # CASE 2) YOB scalar + ageDifferences
    if (is_scalar(YOB) && !is.null(ageDifferences)) {
        YOBv = as.numeric(YOB) + c(0, ageDifferences)
        return(list(YOB = YOBv, ageDifferences = ageDifferences))
    }

    # CASE 3) YOB scalar + agesJoint
    if (is_scalar(YOB) && !is.null(agesJoint)) {
        ageDifferences = agesJoint[-1] - agesJoint[1]
        YOBv = as.numeric(YOB) + c(0, ageDifferences)
        return(list(YOB = YOBv, ageDifferences = ageDifferences))
    }

    # CASE 4) ages vector (first life only) + ageDifferences
    if (is_vector(ages) && length(ages) >= 1L && !is.null(ageDifferences)) {
        return(list(YOB = NULL, ageDifferences = ageDifferences))
    }

    # CASE 5) agesJoint vector (individual ages)
    if (is_vector(agesJoint) && length(agesJoint) > 1L) {
        aj <- as.numeric(agesJoint)
        return(list(YOB = NULL, ageDifferences = aj[-1] - aj[1]))
    }

    # CASE 6) ageDifferences only
    if (!is.null(ageDifferences)) {
        return(list(YOB = NULL, ageDifferences = ageDifferences))
    }

    stop("Could not normalize joint ages: provide YOB, agesJoint, or ageDifferences in one of the supported combinations.")
}


#' @describeIn deathProbabilities
#'   Return the (cohort) death probabilities of the life table given the birth year (if needed)
#'
#' @details
#' The ages and years of birth of the joint lives can be given in multiple arguments. They will be evaluated in the following order:
#' \itemize{
#'   \item \code{YOB}: vector of birth years for all joint lives
#'   \item \code{YOB} (scalar, for the first life) and \code{ageDifferences}: relative to the first life
#'   \item \code{YOB} (scalar, for the first life) and \code{agesJoint}: explicit list of all ages of the joint lives
#'   \item \code{ages} (vector of requested ages, for the first life ONLY) and \code{ageDifferences}: relative to the first life
#'   \item \code{agesJoint}: numeric vector of individual ages
#'   \item \code{ageDifferences} only (not permitted for cohort life tables)
#' }
#'
#' @param ages The vector of desired ages in the return value. This refers to the
#'             age of the first person in the joint-life status. The ages of the
#'             other persons will follow the given ageDifferences or YOB or age
#'             vectors
#' @param age The vector of ages of the persons under consideration
#' @param ageDifferences A vector of age differences (relative to the first person) of all joint lives.
#' @param YOB The vector of birth-years, or the birth-year of the first person.
#'
#' @examples
#' mortalityTables.load("Germany_Census")
#' table.JL = mortalityTable.jointLives(
#'     name = "ADSt 24/26 auf verbundene Leben",
#'     table = mort.DE.census.1924.26.male
#' )
#' deathProbabilities(table.JL, YOB = 1977, ageDifferences = c(1, 5, -5, 16))
#' deathProbabilities(table.JL, YOB = 1977, ageDifferences = c(0))
#' deathProbabilities(table.JL, YOB = 1977, ageDifferences = c(1, 5, 16))
#'
setMethod("deathProbabilities", "mortalityTable.jointLives",
          function(object,  ..., agesJoint = NULL, ageDifferences = NULL, ages = NULL, YOB = 1975) {
              ageInfo = normalizeJointAges(YOB = YOB, ages = ages, ageDifferences = ageDifferences, agesJoint = agesJoint);
              qxMatrix = deathProbabilitiesIndividual(c(object@table), YOB = ageInfo$YOB, ageDifferences = ageInfo$ageDifferences);

              # TODO: Use the ages argument to return only the desired q_xxx

              # First death probabilities are characterized as p_x1x2x3.. = \prod p_xi, i.e.
              # q_x1x2x3... = 1 - \prod (1 - p_xi)
              qx = 1 - apply(1 - qxMatrix, 1, prod)
              qx = qx * (1 + object@loading)
              fillAges(object@modification(qx), givenAges = ages(object), neededAges = ages)
          })

#' @describeIn getOmega Return the maximum age of the joint lives mortality table (returns the maximum age of the first table used for joint lives, as the ages of the joint lives are not known to the function)
setMethod("getOmega", "mortalityTable.jointLives",
          function(object) {
              getOmega(c(object@table)[[1]])
          })


#' @describeIn periodDeathProbabilities
#' Return the (period) death probabilities of the joint lives mortality table for a given observation year
#'
#' @details
#' The ages and years of birth of the joint lives can be given in multiple arguments. They will be evaluated in the following order:
#' \itemize{
#'   \item \code{ages} (vector of requested ages, for the first life ONLY) and \code{ageDifferences}: relative to the first life
#'   \item \code{agesJoint}: numeric vector of individual ages
#'   \item \code{agesJoint} (scalar, age of only the first life) and \code{ageDifferences} (relative to the first life)
#'   \item \code{ageDifferences} only
#' }
#'

#' @param ageDifferences A vector of age differences of all joint lives.
#'
#' @examples
#' mortalityTables.load("Germany_Census")
#' table.JL = mortalityTable.jointLives(
#'     name = "ADSt 24/26 auf verbundene Leben",
#'     table = mort.DE.census.1924.26.male
#' )
#' periodDeathProbabilities(table.JL, Period = 2017, ageDifferences = c(1, 5, -5, 16))
#' periodDeathProbabilities(table.JL, Period = 2017, ageDifferences = c(0))
#' periodDeathProbabilities(table.JL, Period = 2017, ageDifferences = c(1, 5, 16))
#'

setMethod("periodDeathProbabilities", "mortalityTable.jointLives",
          function(object,  ..., agesJoint = NULL, ageDifferences = NULL, ages = NULL, Period = 1975) {
              qxMatrix = periodDeathProbabilitiesIndividual(c(object@table), period = Period, ageDifferences = ageDifferences);
              # First death probabilities are characterized as p_x1x2x3.. = \prod p_xi, i.e.
              # q_x1x2x3... = 1 - \prod (1 - p_xi)
              qx = 1 - apply(1 - qxMatrix, 1, prod)
              qx = qx * (1 + object@loading)
              fillAges(object@modification(qx), givenAges = ages(object), neededAges = ages)
          })


# Examples
if (FALSE) {
    mortalityTables.load("Germany_Census")
    table.JL = mortalityTable.jointLives(
        name = "ADSt 24/26 auf verbundene Leben",
        table = mort.DE.census.1924.26.male
    )
    deathProbabilities(table.JL, YOB = 1977, ageDifferences = c(1, 5, -5, 16))
    deathProbabilities(table.JL, ageDifferences = c(0))
    deathProbabilities(table.JL, ageDifferences = c(1, 5, 16))

}
