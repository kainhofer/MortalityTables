stopifnot(require(methods), require(utils), require(MortalityTables))


###############################################################################
# DAV 2004R exact (Male, Female)
###############################################################################

DAV2004R.data.basistafeln = utils::read.csv(
    system.file("extdata",
                "Germany_Annuities_DAV2004R.csv",
                package = "MortalityTables"),
    col.names = c("age",
                  "qxSel2Ord", "qySel2Ord", "qxAgg2Ord", "qyAgg2Ord",
                  "qxSelBestand", "qySelBestand", "qxAggBestand", "qyAggBestand",
                  "qxSel", "qySel", "qxAgg", "qyAgg"),
    row.names = NULL,
    skip = 3)


DAV2004R.data.trend = utils::read.csv(
    system.file("extdata",
                "Germany_Annuities_DAV2004R_Trends.csv",
                package = "MortalityTables"),
    col.names = c("age",
                  "trend2Ord.male.start", "trend2Ord.female.start",
                  "trend2Ord.male.end", "trend2Ord.female.end",
                  "trendBestand.male.start", "trendBestand.female.start",
                  "trendBestand.male.end", "trendBestand.female.end",
                  "trend1Ord.male", "trend1Ord.female"),
    row.names = NULL,
    skip = 3)


DAV2004R.data.select = utils::read.csv(
    system.file("extdata",
                "Germany_Annuities_DAV2004R_Select.csv",
                package = "MortalityTables"),
    col.names = c("year", "SelectMale", "SelectFemale"),
    row.names = NULL,
    skip = 2)


DAV2004R.data.av.grundtafeln = utils::read.csv(
    system.file("extdata",
                "Germany_Annuities_DAV2004R_AVBase.csv",
                package = "MortalityTables"),
    col.names = c("age",
                  "qxBestand", "qyBestand",
                  "qxB20", "qyB20",
                  "qx1Ord", "qy1Ord"),
    row.names = NULL,
    skip = 2)


DAV2004R.data.av = utils::read.csv(
    system.file("extdata",
                "Germany_Annuities_DAV2004R_AV.csv",
                package = "MortalityTables"),
    col.names = c("YOB", "shiftMBestand", "shiftFBestand",
                  "shiftMB20", "shiftFB20",
                  "shiftM1Ord", "shiftF1Ord"),
    row.names = 1,
    skip = 1)


# colnames(DAV2004R.data.basistafeln)
# colnames(DAV2004R.data.trend)

DAV2004R.male = mortalityTable.trendProjection(
    name = "DAV 2004R male, aggregate, loaded",
    ages = DAV2004R.data.basistafeln$age,
    baseYear = 1999,
    deathProbs = DAV2004R.data.basistafeln$qxAgg,
    trend = DAV2004R.data.trend$trend1Ord.male
);

DAV2004R.female = mortalityTable.trendProjection(
    name = "DAV 2004R female, aggregate, loaded",
    ages = DAV2004R.data.basistafeln$age,
    baseYear = 1999,
    deathProbs = DAV2004R.data.basistafeln$qyAgg,
    trend = DAV2004R.data.trend$trend1Ord.female
)


DAV2004R.male.2Ord = mortalityTable.trendProjection(
    name = "DAV 2004R male, aggregate, unloaded, no trend dampening",
    ages = DAV2004R.data.basistafeln$age,
    baseYear = 1999,
    deathProbs = DAV2004R.data.basistafeln$qxAgg,
    trend = DAV2004R.data.trend$trend2Ord.male.start
);

DAV2004R.female.2Ord = mortalityTable.trendProjection(
    name = "DAV 2004R female, aggregate, unloaded, no trend dampening",
    ages = DAV2004R.data.basistafeln$age,
    baseYear = 1999,
    deathProbs = DAV2004R.data.basistafeln$qyAgg,
    trend = DAV2004R.data.trend$trend2Ord.female.start
)



DAV2004R.male.av = mortalityTable.ageShift(
    name = "DAV 2004R male, age-shifted, aggregate, loaded",
    ages = DAV2004R.data.av.grundtafeln$age,
    deathProbs = DAV2004R.data.av.grundtafeln$qx1Ord,
    ageShifts = DAV2004R.data.av["shiftM1Ord"]
);
DAV2004R.female.av = mortalityTable.ageShift(
    name = "DAV 2004R female, age-shifted, aggregate, loaded",
    ages = DAV2004R.data.av.grundtafeln$age,
    deathProbs = DAV2004R.data.av.grundtafeln$qy1Ord,
    ageShifts = DAV2004R.data.av["shiftF1Ord"]
)

DAV2004R.male.av.2Ord = mortalityTable.ageShift(
    name = "DAV 2004R male, age-shifted, aggregate, unloaded, no trend dampening",
    ages = DAV2004R.data.av.grundtafeln$age,
    deathProbs = DAV2004R.data.av.grundtafeln$qxBestand,
    ageShifts = DAV2004R.data.av["shiftMBestand"]
);
DAV2004R.female.av.2Ord = mortalityTable.ageShift(
    name = "DAV 2004R female, age-shifted, aggregate, unloaded, no trend dampening",
    ages = DAV2004R.data.av.grundtafeln$age,
    deathProbs = DAV2004R.data.av.grundtafeln$qyBestand,
    ageShifts = DAV2004R.data.av["shiftFBestand"]
)


rm(DAV2004R.data.basistafeln,
   DAV2004R.data.trend,
   DAV2004R.data.select,
   DAV2004R.data.av.grundtafeln,
   DAV2004R.data.av)

