stopifnot(require(methods), require(utils), require(MortalityTables)) # MortalityTable classes; new; Excel reader


###############################################################################
# AVÖ 1996R exact (Male, Female), 1st-order only
###############################################################################

AVOe1996R.exakt.data = utils::read.csv(
    system.file("extdata",
                "Austria_Annuities_AVOe1996R.csv",
                package="MortalityTables"),
    skip=2)

AVOe1996R.trend.switching = function(year) {
    if (year <= 1971) {
        15/(1991-year)
    } else if (1971 < year && year < 1981) {
        1+(year-1981)^2/(year-1991)/20
    } else if (1981 <= year && year <= 2000) {
        1
    } else if (2000 < year && year < 2010) {
        1-(year-2000)^2/(year-1991)/20
    } else if (year >= 2010) {
        14/(year-1991)
    }
}

AVOe1996R.male = mortalityTable.trendProjection(
    name = "AVÖ 1996R male",
    ages = AVOe1996R.exakt.data$age, baseYear = 1991,
    deathProbs = AVOe1996R.exakt.data$qx1991 * AVOe1996R.exakt.data$factorM,
    trend = AVOe1996R.exakt.data$trendM.long,
    trend2 = AVOe1996R.exakt.data$trendM.short,
    dampingFunction = AVOe1996R.trend.switching
);

AVOe1996R.female = mortalityTable.trendProjection(
    name = "AVÖ 1996R female",
    ages = AVOe1996R.exakt.data$age, baseYear = 1991,
    deathProbs = AVOe1996R.exakt.data$qy1991 * AVOe1996R.exakt.data$factorF,
    trend = AVOe1996R.exakt.data$trendF.long,
    trend2 = AVOe1996R.exakt.data$trendF.short,
    dampingFunction = AVOe1996R.trend.switching
);

AVOe1996R.male.group = mortalityTable.trendProjection(
    name = "AVÖ 1996R male, group",
    ages = AVOe1996R.exakt.data$age, baseYear = 1991,
    deathProbs = AVOe1996R.exakt.data$qx1991 * AVOe1996R.exakt.data$factorMG,
    trend = AVOe1996R.exakt.data$trendM.long,
    trend2 = AVOe1996R.exakt.data$trendM.short,
    dampingFunction = AVOe1996R.trend.switching
);

AVOe1996R.female.group = mortalityTable.trendProjection(
    name = "AVÖ 1996R female, group",
    ages = AVOe1996R.exakt.data$age, baseYear = 1991,
    deathProbs = AVOe1996R.exakt.data$qy1991 * AVOe1996R.exakt.data$factorFG,
    trend = AVOe1996R.exakt.data$trendF.long,
    trend2 = AVOe1996R.exakt.data$trendF.short,
    dampingFunction = AVOe1996R.trend.switching
);

rm(AVOe1996R.exakt.data)

