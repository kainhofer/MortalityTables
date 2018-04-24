stopifnot(require(methods), require(utils), require(MortalityTables)) # MortalityTable classes; new; Excel reader


###############################################################################
### Gesamtbevölkerung Österreich: Bevölkerungsprognose bis 2080 (mittleres Szenario)
### Datenquelle: Statistik Austria
###############################################################################


AT.pop.fc = utils::read.csv(system.file("extdata", "Austria_Population_Forecast.csv", package = "MortalityTables"), skip = 2);

mort.AT.forecast.male = mortalityTable.trendProjection(
    name = "Österreich Männer (mittl. Sz.)",
    baseYear = 2014,
    deathProbs = AT.pop.fc$q.Männer.2014,
    trend = -AT.pop.fc$Männer,
    ages = AT.pop.fc$X
)
mort.AT.forecast.female = mortalityTable.trendProjection(
    name = "Österreich Frauen (mittl. Sz.)",
    baseYear = 2014,
    deathProbs = AT.pop.fc$q.Frauen.2014,
    trend = -AT.pop.fc$Frauen,
    ages = AT.pop.fc$X
)

rm(AT.pop.fc)

###############################################################################

# mortalityTables.load("Austria*")
# plot(mort.AT.forecast.male, mort.AT.forecast.female, AVOe1996R.male, AVOe2005R.male, AVOe1996R.female, AVOe2005R.female, YOB = 2000)
# plotMortalityTrend(mort.AT.forecast.male, mort.AT.forecast.female, AVOe1996R.male, AVOe2005R.male, AVOe1996R.female, AVOe2005R.female, Period = 2002)
