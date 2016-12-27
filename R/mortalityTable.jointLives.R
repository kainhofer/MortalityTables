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
    pad0(v, l, tail(v, n = 1))
}

#' @export
deathProbabilitiesIndividual = function(tables, YOB, ageDifferences) {
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
                qxtmp = tail(qx, -difference);
            }
            qxnew = padLast(qxtmp, qxlen)
            str(qxnew);
            qxnew
        },
        tables, YOB, ageDifferences);
    qxMatrix
}

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
                qxtmp = tail(qx, -difference);
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

#' @describeIn deathProbabilities Return the (cohort) death probabilities of the
#'                                life table given the birth year (if needed)
setMethod("deathProbabilities", "mortalityTable.jointLives",
          function(object,  ..., ageDifferences = c(), YOB = 1975) {
              qxMatrix = deathProbabilitiesIndividual(c(object@table), YOB = YOB, ageDifferences = ageDifferences);
              # First death probabilities are characterized as p_x1x2x3.. = \prod p_xi, i.e.
              # q_x1x2x3... = 1 - \prod (1 - p_xi)
              qx = 1 - apply(1 - qxMatrix, 1, prod)
              object@modification(qx * (1 + object@loading));
          })

#' @describeIn getOmega Return the maximum age of the joint lives mortality table (returns the maximum age of the first table used for joint lives, as the ages of the joint lives are now known to the function)
setMethod("getOmega", "mortalityTable.jointLives",
          function(object) {
              getOmega(c(object@table)[[1]])
          })


#' @describeIn periodDeathProbabilities Return the (period) death probabilities
#'             of the joint lives mortality table for a given observation year
setMethod("periodDeathProbabilities", "mortalityTable.jointLives",
          function(object,  ..., ageDifferences = c(), Period = 1975) {
              qxMatrix = periodDeathProbabilitiesIndividual(c(object@table), period = Period, ageDifferences = ageDifferences);
              # First death probabilities are characterized as p_x1x2x3.. = \prod p_xi, i.e.
              # q_x1x2x3... = 1 - \prod (1 - p_xi)
              qx = 1 - apply(1 - qxMatrix, 1, prod)
              # Cut to same length as ages:
              ages = ages(object);
              qx = qx[1:length(ages)];
              object@modification(qx * (1 + object@loading));
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
