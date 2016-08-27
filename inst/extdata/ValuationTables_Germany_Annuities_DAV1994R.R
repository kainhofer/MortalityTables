stopifnot(require(methods), require(utils), require(ValuationTables))

###############################################################################
# DAV 1994R exact (Male, Female), 1st-order only
###############################################################################


DAV1994R.exakt.data = utils::read.csv(
    system.file("extdata",
                "Germany_Annuities_DAV1994R.csv",
                package = "ValuationTables"),
    col.names = c("age",
                  "qx2000", "qy2000",
                  "trendM", "trendF",
                  "qxAVbase1955", "qyAVbase1955",
                  "", "", "", "", "", "", ""),
    row.names = NULL,
    skip = 2)

DAV1994R.av.data = utils::read.csv(
    system.file("extdata",
                "Germany_Annuities_DAV1994R_AV.csv",
                package = "ValuationTables"),
    row.names = 1,
    skip = 4
)

DAV1994R.male = valuationTable.trendProjection(
    name = "DAV 1994R male",
    ages = DAV1994R.exakt.data$age,
    baseYear = 2000,
    deathProbs = DAV1994R.exakt.data$qx2000,
    trend = DAV1994R.exakt.data$trendM
);

DAV1994R.female = valuationTable.trendProjection(
    name = "DAV 1994R female",
    ages = DAV1994R.exakt.data$age,
    baseYear = 2000,
    deathProbs = DAV1994R.exakt.data$qxAVbase1955,
    trend = DAV1994R.exakt.data$trendF
);



DAV1994R.male.av = valuationTable.ageShift(
    name = "DAV 1994R male, age-shifted",
    ages = DAV1994R.exakt.data$age,
    deathProbs = DAV1994R.exakt.data$qxAVbase1955,
    ageShifts = DAV1994R.av.data["AS.Male"]
);

DAV1994R.female.av = valuationTable.ageShift(
    name = "DAV 1994R female, age-shifted",
    ages = DAV1994R.exakt.data$age,
    deathProbs = DAV1994R.exakt.data$qyAVbase1955,
    ageShifts = DAV1994R.av.data["AS.Female"]
);


rm(DAV1994R.exakt.data, DAV1994R.av.data)

# plotValuationTables(DAV1994R.male, DAV1994R.male.av, DAV1994T.male, DAV1994T.male.2Ord, DAV2008T.male)
