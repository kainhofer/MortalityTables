stopifnot(require(methods), require(utils), require(MortalityTables)) # MortalityTable classes; new; Excel reader


###############################################################################
### Gesamtbevölkerung Österreich: Bevölkerungsprognose bis 2080 (mittleres Szenario)
### Datenquelle: Statistik Austria
###############################################################################


AT.pop.obs.M = utils::read.csv(system.file("extdata", "Austria_Population_Observation_M.csv", package = "MortalityTables"), check.names = FALSE, row.names = 1);
AT.pop.obs.F = utils::read.csv(system.file("extdata", "Austria_Population_Observation_F.csv", package = "MortalityTables"), check.names = FALSE, row.names = 1);
AT.pop.obs.U = utils::read.csv(system.file("extdata", "Austria_Population_Observation_U.csv", package = "MortalityTables"), check.names = FALSE, row.names = 1);

mort.AT.observed.male = mortalityTable.observed(
    name = "Österreich Männer Beobachtung",
    deathProbs = AT.pop.obs.M,
    ages = as.integer(rownames(AT.pop.obs.M)),
    years = as.integer(colnames(AT.pop.obs.M)),
    data = list(
        dim = list(sex = "m", collar = "Gesamtbevölkerung", type = "Beobachtung", data = "official", year = "1947-2017")
    )
)
mort.AT.observed.female = mortalityTable.observed(
    name = "Österreich Frauen Beobachtung",
    deathProbs = AT.pop.obs.F,
    ages = as.integer(rownames(AT.pop.obs.F)),
    years = as.integer(colnames(AT.pop.obs.F)),
    data = list(
        dim = list(sex = "f", collar = "Gesamtbevölkerung", type = "Beobachtung", data = "official", year = "1947-2017")
    )
)
mort.AT.observed.unisex = mortalityTable.observed(
    name = "Österreich Unisex Beobachtung",
    deathProbs = AT.pop.obs.U,
    ages = as.integer(rownames(AT.pop.obs.U)),
    years = as.integer(colnames(AT.pop.obs.U)),
    data = list(
        dim = list(sex = "u", collar = "Gesamtbevölkerung", type = "Beobachtung", data = "official", year = "1947-2017")
    )
)

rm(AT.pop.obs.M, AT.pop.obs.F, AT.pop.obs.U)

###############################################################################

# mortalityTables.load("Austria*")
# plot(mort.AT.forecast.male, mort.AT.forecast.female, AVOe1996R.male, AVOe2005R.male, AVOe1996R.female, AVOe2005R.female, YOB = 2000)
# plotMortalityTrend(mort.AT.forecast.male, mort.AT.forecast.female, AVOe1996R.male, AVOe2005R.male, AVOe1996R.female, AVOe2005R.female, Period = 2002)
