stopifnot(require(methods), require(utils), require(ValuationTables)) # ValuationTable classes; new; Excel reader

###############################################################################
### EROM/EROF 85 and G 1985 (period and age-shifted generation)
###############################################################################


eromf.data = utils::read.csv(
    system.file(
        "extdata",
        "Austria_Annuities_EROMF.csv",
        package="ValuationTables"),
    skip=2)
eromf.data.av = utils::read.csv(
    system.file(
        "extdata",
        "Austria_Annuities_EROMF_AV.csv",
        package="ValuationTables"),
    skip=2)



EROM85.male = valuationTable.period(
    name = "EROM 85, male",
    ages = eromf.data$Alter,
    deathProbs = eromf.data$EROM.85
);

EROF85.female = valuationTable.period(
    name = "EROF 85, female",
    ages = eromf.data$Alter,
    deathProbs = eromf.data$EROF.85
);

EROM.G1950.male = valuationTable.period(
    name = "EROM G 1950 Basistafel, male",
    ages = eromf.data$Alter,
    deathProbs = eromf.data$EROM.G1950,
    baseYear = 1950
);

EROF.G1950.female = valuationTable.period(
    name = "EROF G 1950 Basistafel, female",
    ages = eromf.data$Alter,
    deathProbs = eromf.data$EROF.G1950,
    baseYear = 1950
);


EROM.G1950.male.av = valuationTable.ageShift(
    name = "EROM G 1950 mit Altersverschiebung, male",
    ages = eromf.data$Alter,
    deathProbs = eromf.data$EROM.G1950,
    ageShifts = eromf.data.av["Shift.M"],
    baseYear = 1950
);
EROF.G1950.female.av = valuationTable.ageShift(
    name = "EROF G 1950 mit Altersverschiebung, female",
    ages = eromf.data$Alter,
    deathProbs = eromf.data$EROF.G1950,
    ageShifts = eromf.data.av["Shift.F"],
    baseYear = 1950
);

rm(eromf.data, eromf.data.av)

