stopifnot(require(methods), require(utils), require(MortalityTables)) # MortalityTable classes; new; Excel reader


###############################################################################
### Gesamtbevölkerung Österreich: Bevölkerungsprognose bis 2080 (mittleres Szenario)
### Datenquelle: Statistik Austria
###############################################################################


AT.pop.fc = utils::read.csv(system.file("extdata", "Austria_Population_Forecast.csv", package = "MortalityTables"), skip = 2, encoding = "UTF-8");

mort.AT.forecast.male = mortalityTable.trendProjection(
    name = "Österreich Männer (mittl. Sz.)",
    baseYear = 2014,
    deathProbs = AT.pop.fc$q.M.2014,
    trend = -AT.pop.fc$M,
    ages = AT.pop.fc$X,
    data = list(
        dim = list(sex = "m", collar = "Gesamtbevölkerung", type = "Bevölkerungsprognose", data = "official", year = "2014-2080")
    )
)
mort.AT.forecast.female = mortalityTable.trendProjection(
    name = "Österreich Frauen (mittl. Sz.)",
    baseYear = 2014,
    deathProbs = AT.pop.fc$q.F.2014,
    trend = -AT.pop.fc$F,
    ages = AT.pop.fc$X,
    data = list(
        dim = list(sex = "w", collar = "Gesamtbevölkerung", type = "Bevölkerungsprognose", data = "official", year = "2014-2080")
    )
)


mort.AT.forecast = array(
    data = c(mortalityTable.NA),
    dim = c(2),
    dimnames = list(Geschlecht = c("m", "w"))
)
mort.AT.forecast[["m"]] = mort.AT.forecast.male
mort.AT.forecast[["w"]] = mort.AT.forecast.female



rm(AT.pop.fc)

###############################################################################

# mortalityTables.load("Austria*")
# plot(mort.AT.forecast.male, mort.AT.forecast.female, AVOe1996R.male, AVOe2005R.male, AVOe1996R.female, AVOe2005R.female, YOB = 2000)
# plotMortalityTrend(mort.AT.forecast.male, mort.AT.forecast.female, AVOe1996R.male, AVOe2005R.male, AVOe1996R.female, AVOe2005R.female, Period = 2002)
