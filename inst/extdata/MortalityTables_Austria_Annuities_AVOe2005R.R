stopifnot(require(methods), require(utils), require(MortalityTables)) # MortalityTable classes; new; Excel reader


###############################################################################
# AVÖ 2005R exact (Male, Female, unisex)
# gender-specific tables also have 2nd-order tables, unisex only 1st-order table
###############################################################################

AVOe2005R.exakt.data = utils::read.csv(
    system.file(
        "extdata",
        "Austria_Annuities_AVOe2005R.csv",
        package = "MortalityTables"),
    skip = 2);

AVOe2005R.trend.damping = function(t) {
    100*atan(t/100)
}
AVOe2005R_gen = function(nm, probs, trend, sex = "m", data = "loaded") {
    with(
        AVOe2005R.exakt.data,
        mortalityTable.trendProjection(
            name = nm,
            ages = age,
            baseYear = 2001,
            deathProbs = AVOe2005R.exakt.data[[probs]],
            trend = AVOe2005R.exakt.data[[trend]],
            dampingFunction = AVOe2005R.trend.damping,
            data = list(
                dim = list(sex = sex, collar = "Rententafel", type = "Rententafel Österreich", data = data, year = "AVÖ 2005-R")
            )
        )
    )
}

AVOe2005R.male            = AVOe2005R_gen("AVÖ 2005R male (exact), loaded",   "qx2001", "trendM", sex = "m", data = "loaded");
AVOe2005R.female          = AVOe2005R_gen("AVÖ 2005R female (exact), loaded", "qy2001", "trendF", sex = "w", data = "loaded");
AVOe2005R.unisex          = AVOe2005R_gen("AVÖ 2005R unisex (exact), loaded", "qu2001", "trendU", sex = "u", data = "loaded");
AVOe2005R.male.unloaded   = AVOe2005R_gen("AVÖ 2005R male (exact), unloaded",   "qx2001.2Ord", "trendM.2Ord", sex = "m", data = "unloaded");
AVOe2005R.female.unloaded = AVOe2005R_gen("AVÖ 2005R female (exact), unloaded", "qy2001.2Ord", "trendF.2Ord", sex = "w", data = "unloaded");
AVOe2005R.male.group      = AVOe2005R_gen("AVÖ 2005R male group (exact), loaded",   "qx2001G", "trendM", sex = "m", data = "loaded, group");
AVOe2005R.female.group    = AVOe2005R_gen("AVÖ 2005R female group (exact), loaded", "qy2001G", "trendF", sex = "w", data = "loaded, group");
AVOe2005R.unisex.group    = AVOe2005R_gen("AVÖ 2005R unisex group (exact), loaded", "qu2001G", "trendU", sex = "u", data = "loaded, group");

AVOe2005R.male.nodamping            = mT.setDimInfo(undampenTrend(AVOe2005R.male), data = "loaded, no trend damping")
AVOe2005R.female.nodamping          = mT.setDimInfo(undampenTrend(AVOe2005R.female), data = "loaded, no trend damping");
AVOe2005R.unisex.nodamping          = mT.setDimInfo(undampenTrend(AVOe2005R.unisex), data = "loaded, no trend damping");
AVOe2005R.male.nodamping.unloaded   = mT.setDimInfo(undampenTrend(AVOe2005R.male.unloaded), data = "unloaded, no trend damping");
AVOe2005R.female.nodamping.unloaded = mT.setDimInfo(undampenTrend(AVOe2005R.female.unloaded), data = "unloaded, no trend damping");
AVOe2005R.male.nodamping.group      = mT.setDimInfo(undampenTrend(AVOe2005R.male.group), data = "loaded, group, no trend damping");
AVOe2005R.female.nodamping.group    = mT.setDimInfo(undampenTrend(AVOe2005R.female.group), data = "loaded, group, no trend damping");
AVOe2005R.unisex.nodamping.group    = mT.setDimInfo(undampenTrend(AVOe2005R.unisex.group), data = "loaded, group, no trend damping");


###############################################################################
#AVÖ 2005R with age-shifting (Male, Female, unisex), 1st-order only
###############################################################################


AVOe2005R.av.base = utils::read.csv(
    system.file(
        "extdata",
        "Austria_Annuities_AVOe2005R_AVBasis.csv",
        package = "MortalityTables"),
    skip = 2);
AVOe2005R.av.verschiebung = utils::read.csv(
    system.file(
        "extdata",
        "Austria_Annuities_AVOe2005R_AVShifts.csv",
        package = "MortalityTables"),
    row.names = 1,
    skip = 2);

AVOe2005R_gen.av = function(nm, probs, shft, sex = "m", data = "age-shift, loaded") {
    mortalityTable.ageShift(
        name = nm,
        ages = AVOe2005R.av.base$age,
        deathProbs = AVOe2005R.av.base[[probs]],
        ageShifts = na.omit(AVOe2005R.av.verschiebung[shft]),
        data = list(
            dim = list(sex = sex, collar = "Rententafel", type = "Rententafel Österreich", data = data, year = "AVÖ 2005-R")
        )
    )
}

AVOe2005R.male.av = AVOe2005R_gen.av(
    "AVÖ 2005R male (age-shifted), loaded",
    "qx1965", "shiftM", sex = "m", data = "age-shift, loaded");
AVOe2005R.female.av = AVOe2005R_gen.av(
    "AVÖ 2005R female (age-shifted), loaded",
    "qy1965", "shiftF", sex = "w", data = "age-shift, loaded");
AVOe2005R.unisex.av = AVOe2005R_gen.av(
    "AVÖ 2005R unisex (age-shifted), loaded",
    "qu1972", "shiftU", sex = "u", data = "age-shift, loaded");
AVOe2005R.male.group.av = AVOe2005R_gen.av(
    "AVÖ 2005R male group (age-shifted), loaded",
    "qx1965G", "shiftMG", sex = "m", data = "age-shift, loaded, group");
AVOe2005R.female.group.av = AVOe2005R_gen.av(
    "AVÖ 2005R female group (age-shifted), loaded",
    "qy1965G", "shiftFG", sex = "w", data = "age-shift, loaded, group");
AVOe2005R.unisex.group.av = AVOe2005R_gen.av(
    "AVÖ 2005R unisex group (age-shifted), loaded",
    "qu1972G", "shiftUG", sex = "u", data = "age-shift, loaded, group")



###############################################################################

# options("scipen" = 3, "digits" = 10)
# t = AVOe2005R.unisex;
# deathProbabilities(t, YOB = 1981)

# plot(mort.AT.census.1869.male, mort.AT.census.1869.female, mort.AT.census.2011.male, mort.AT.census.2011.female, AVOe2005R.male, AVOe2005R.female, YOB = 1972,title = "Vergleich österreichische Sterbetafeln, YOB = 1972 (bei Generationentafeln)")
#
# plot(mort.AT.census.2001.male, AVOe2005R.male, YOB = 1972, title = "Vergleich österreichische Sterbetafeln")
# plot(getCohortTable(AVOe2005R.male, YOB = 1972), getCohortTable(AVOe2005R.male, YOB = 2016), title = "Vergleich österreichische Sterbetafeln")

# makeQxDataFrame(mort.AT.census.1869.male, mort.AT.census.1869.female, mort.AT.census.2011.male, mort.AT.census.2011.female, AVOe2005R.male, AVOe2005R.female, YOB = 1972)
# makeQxDataFrame()
