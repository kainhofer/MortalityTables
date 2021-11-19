stopifnot(require(methods), require(utils), require(MortalityTables)) # MortalityTable classes; new; Excel reader

###############################################################################
### EROM/EROF 85 and G 1985 (period and age-shifted generation)
###############################################################################


eromf.data = utils::read.csv(
    system.file(
        "extdata",
        "Austria_Annuities_EROMF.csv",
        package = "MortalityTables"),
    skip = 2)
eromf.data.av = utils::read.csv(
    system.file(
        "extdata",
        "Austria_Annuities_EROMF_AV.csv",
        package = "MortalityTables"),
    skip = 2)
rownames(eromf.data.av) = eromf.data.av$YOB


EROM85.male = mortalityTable.period(
    name = "EROM 85, male",
    ages = eromf.data$Alter,
    deathProbs = eromf.data$EROM.85,
    data = list(
        dim = list(sex = "m", collar = "Rententafel", type = "Rententafel Österreich", data = "unloaded", year = "EROM 85")
    )
);

EROF85.female = mortalityTable.period(
    name = "EROF 85, female",
    ages = eromf.data$Alter,
    deathProbs = eromf.data$EROF.85,
    data = list(
        dim = list(sex = "w", collar = "Rententafel", type = "Rententafel Österreich", data = "unloaded", year = "EROF 85")
    )
);

EROM.G1950.male = mortalityTable.period(
    name = "EROM G 1950 Basistafel, male",
    ages = eromf.data$Alter,
    deathProbs = eromf.data$EROM.G1950,
    baseYear = 1950,
    data = list(
        dim = list(sex = "m", collar = "Rententafel", type = "Rententafel Österreich", data = "unloaded", year = "EROM G1950")
    )
);

EROF.G1950.female = mortalityTable.period(
    name = "EROF G 1950 Basistafel, female",
    ages = eromf.data$Alter,
    deathProbs = eromf.data$EROF.G1950,
    baseYear = 1950,
    data = list(
        dim = list(sex = "w", collar = "Rententafel", type = "Rententafel Österreich", data = "unloaded", year = "EROF G1950")
    )
);


EROM.G1950.male.av = mortalityTable.ageShift(
    name = "EROM G 1950 mit Altersverschiebung, male",
    ages = eromf.data$Alter,
    deathProbs = eromf.data$EROM.G1950,
    ageShifts = eromf.data.av["Shift.M"],
    baseYear = 1950,
    data = list(
        dim = list(sex = "m", collar = "Rententafel", type = "Rententafel Österreich", data = "age-shifted", year = "EROM G1950 AV")
    )
);
EROF.G1950.female.av = mortalityTable.ageShift(
    name = "EROF G 1950 mit Altersverschiebung, female",
    ages = eromf.data$Alter,
    deathProbs = eromf.data$EROF.G1950,
    ageShifts = eromf.data.av["Shift.F"],
    baseYear = 1950,
    data = list(
        dim = list(sex = "w", collar = "Rententafel", type = "Rententafel Österreich", data = "age-shifted", year = "EROF G1950 AV")
    )
);

rm(eromf.data, eromf.data.av)

