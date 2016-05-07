stopifnot(require(methods), require(utils), require(ValuationTables)) # ValuationTable classes; new; Excel reader


###############################################################################
### RR67 Rententafel für Männer, 3%
###############################################################################

rr67.data = utils::read.csv("Tables/Austria_Annuities_RR67.csv", skip=2)

rr67 = ValuationTables::valuationTable_period(
  name = "ÖVM 59/61 RR67", ages = rr67.data$Alter, deathProbs = rr67.data$qx
);
rm(rr67.data);


###############################################################################
### EROM/EROF 85 and G 1985 (period and age-shifted generation)
###############################################################################

# eromf.data = openxlsx::read.xlsx("Tables/AVOe_R.xlsx", sheet = "EROM-F Basistafeln", startRow = 3)
eromf.data = utils::read.csv("Tables/Austria_Annuities_EROMF.csv", skip=2)

erom85.male = new("valuationTable_period",
  name = "EROM 85, male", ages = eromf.data$Alter, deathProbs = eromf.data$EROM.85
);
erom85.female = new("valuationTable_period",
  name = "EROF 85, female", ages = eromf.data$Alter, deathProbs = eromf.data$EROF.85
);

EROM.G1950.male = new("valuationTable_period",
  name = "EROM G 1950 Basistafel, male",
  ages = eromf.data$Alter,
  deathProbs = eromf.data$EROM.G1950,
  baseYear = 1950
);
EROF.G1950.female = new("valuationTable_period",
  name = "EROF G 1950 Basistafel, female",
  ages = eromf.data$Alter,
  deathProbs = eromf.data$EROF.G1950,
  baseYear = 1950
);

# eromf.data.av = openxlsx::read.xlsx("Tables/AVOe_R.xlsx", sheet = "EROM-F G AV", startRow = 3, rowNames = TRUE, colNames = TRUE)
eromf.data.av = utils::read.csv("Tables/Austria_Annuities_EROMF_AV.csv", skip=2)

EROM.G1950.male.av = valuationTable_ageShift(
  name = "EROM G 1950 mit Altersverschiebung, male",
  ages = eromf.data$Alter,
  deathProbs = eromf.data$EROM.G1950,
  ageShifts = eromf.data.av["Shift.M"],
  baseYear = 1950
);
EROF.G1950.female.av = valuationTable_ageShift(
  name = "EROF G 1950 mit Altersverschiebung, female",
  ages = eromf.data$Alter,
  deathProbs = eromf.data$EROF.G1950,
  ageShifts = eromf.data.av["Shift.F"],
  baseYear = 1950
);

rm(eromf.data, eromf.data.av)


###############################################################################
# AVÖ 1996R exact (Male, Female), 1st-order only
###############################################################################

# AVOe1996R.exakt.data = openxlsx::read.xlsx("Tables/AVOe_R.xlsx",
  # sheet = "AVOe 1996R exakt", startRow = 3, cols = c(1:6, 8:12));
AVOe1996R.exakt.data = utils::read.csv("Tables/Austria_Annuities_AVOe1996R.csv", skip=2)

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

AVÖ1996R.male = new("valuationTable_trendProjection",
  name = "AVÖ 1996R male",
  ages = AVOe1996R.exakt.data$age, baseYear = 1991,
  deathProbs = AVOe1996R.exakt.data$qx1991 * AVOe1996R.exakt.data$factorM,
  trend = AVOe1996R.exakt.data$trendM.long,
  trend2 = AVOe1996R.exakt.data$trendM.short,
  dampingFunction = AVOe1996R.trend.switching
);
AVÖ1996R.female = new("valuationTable_trendProjection",
  name = "AVÖ 1996R female",
  ages = AVOe1996R.exakt.data$age, baseYear = 1991,
  deathProbs = AVOe1996R.exakt.data$qy1991 * AVOe1996R.exakt.data$factorF,
  trend = AVOe1996R.exakt.data$trendF.long,
  trend2 = AVOe1996R.exakt.data$trendF.short,
  dampingFunction = AVOe1996R.trend.switching
);
AVÖ1996R.male.group = new("valuationTable_trendProjection",
  name = "AVÖ 1996R male, group",
  ages = AVOe1996R.exakt.data$age, baseYear = 1991,
  deathProbs = AVOe1996R.exakt.data$qx1991 * AVOe1996R.exakt.data$factorMG,
  trend = AVOe1996R.exakt.data$trendM.long,
  trend2 = AVOe1996R.exakt.data$trendM.short,
  dampingFunction = AVOe1996R.trend.switching
);
AVÖ1996R.female.group = new("valuationTable_trendProjection",
  name = "AVÖ 1996R female, group",
  ages = AVOe1996R.exakt.data$age, baseYear = 1991,
  deathProbs = AVOe1996R.exakt.data$qy1991 * AVOe1996R.exakt.data$factorFG,
  trend = AVOe1996R.exakt.data$trendF.long,
  trend2 = AVOe1996R.exakt.data$trendF.short,
  dampingFunction = AVOe1996R.trend.switching
);

rm(AVOe1996R.exakt.data)



###############################################################################
# AVÖ 2005R exact (Male, Female, unisex)
# gender-specific tables also have 2nd-order tables, unisex only 1st-order table
###############################################################################

# AVOe2005R.exakt.data = openxlsx::read.xlsx("Tables/AVOe_R.xlsx", sheet = "AVOe 2005R", startRow = 3, cols = c(1:7, 9:14, 16:18));
AVOe2005R.exakt.data = utils::read.csv("Tables/Austria_Annuities_AVOe2005R.csv", skip = 2);

AVOe2005R.trend.damping = function(t) {
  100*atan(t/100)
}
AVOe2005R_gen = function(nm, probs, trend) {
  with(
    AVOe2005R.exakt.data,
    new("valuationTable_trendProjection",
      name = nm,
      ages = age, baseYear = 2001,
      deathProbs = AVOe2005R.exakt.data[[probs]], trend = AVOe2005R.exakt.data[[trend]],
      dampingFunction = AVOe2005R.trend.damping
    )
  )
}

AVOe2005R.male            = AVOe2005R_gen("AVÖ 2005R male (exact), loaded",   "qx2001", "trendM");
AVOe2005R.female          = AVOe2005R_gen("AVÖ 2005R female (exact), loaded", "qy2001", "trendF");
AVOe2005R.unisex          = AVOe2005R_gen("AVÖ 2005R unisex (exact), loaded", "qu2001", "trendU");
AVOe2005R.male.unloaded   = AVOe2005R_gen("AVÖ 2005R male (exact), unloaded",   "qx2001.2Ord", "trendM.2Ord");
AVOe2005R.female.unloaded = AVOe2005R_gen("AVÖ 2005R female (exact), unloaded", "qy2001.2Ord", "trendF.2Ord");
AVOe2005R.male.group      = AVOe2005R_gen("AVÖ 2005R male group (exact), loaded",   "qx2001G", "trendM");
AVOe2005R.female.group    = AVOe2005R_gen("AVÖ 2005R female group (exact), loaded", "qy2001G", "trendF");
AVOe2005R.unisex.group    = AVOe2005R_gen("AVÖ 2005R unisex group (exact), loaded", "qu2001G", "trendU");

AVOe2005R.male.nodamping            = undampenTrend(AVOe2005R.male);
AVOe2005R.female.nodamping          = undampenTrend(AVOe2005R.female);
AVOe2005R.unisex.nodamping          = undampenTrend(AVOe2005R.unisex);
AVOe2005R.male.nodamping.unloaded   = undampenTrend(AVOe2005R.male.unloaded);
AVOe2005R.female.nodamping.unloaded = undampenTrend(AVOe2005R.female.unloaded);
AVOe2005R.male.nodamping.group      = undampenTrend(AVOe2005R.male.group);
AVOe2005R.female.nodamping.group    = undampenTrend(AVOe2005R.female.group);
AVOe2005R.unisex.nodamping.group    = undampenTrend(AVOe2005R.unisex.group);


###############################################################################
#AVÖ 2005R with age-shifting (Male, Female, unisex), 1st-order only
###############################################################################

AVOe2005R.av.base = openxlsx::read.xlsx("Tables/AVOe_R.xlsx", sheet = "AVOe 2005R AV Basistafel", startRow = 3, rowNames = FALSE);
AVOe2005R.av.verschiebung = openxlsx::read.xlsx("Tables/AVOe_R.xlsx", sheet = "AVOe 2005R AV Verschiebung", startRow = 3, rowNames = TRUE);

AVOe2005R_gen.av = function(nm, probs, shft) {
  new("valuationTable_ageShift",
    name = nm,
    ages = AVOe2005R.av.base$age,
    deathProbs = AVOe2005R.av.base[[probs]],
    ageShifts = na.omit(AVOe2005R.av.verschiebung[shft])
  )
}

AVOe2005R.male.av = AVOe2005R_gen.av("AVÖ 2005R male (age-shifted), loaded",   "qx1965", "shiftM");
AVOe2005R.female.av = AVOe2005R_gen.av("AVÖ 2005R female (age-shifted), loaded", "qy1965", "shiftF");
AVOe2005R.unisex.av = AVOe2005R_gen.av("AVÖ 2005R unisex (age-shifted), loaded", "qu1972", "shiftU");
AVOe2005R.male.group.av = AVOe2005R_gen.av("AVÖ 2005R male group (age-shifted), loaded",   "qx1965G", "shiftMG");
AVOe2005R.female.group.av = AVOe2005R_gen.av("AVÖ 2005R female group (age-shifted), loaded", "qy1965G", "shiftFG");
AVOe2005R.unisex.group.av = AVOe2005R_gen.av("AVÖ 2005R unisex group (age-shifted), loaded", "qu1972G", "shiftUG");



###############################################################################

# options("scipen" = 3, "digits" = 10)
# t = AVOe2005R.unisex;
# deathProbabilities(t, YOB = 1981)

# plotValuationTables(mort.AT.census.1869.male, mort.AT.census.1869.female, mort.AT.census.2011.male, mort.AT.census.2011.female, AVOe2005R.male, AVOe2005R.female, YOB = 1972,title = "Vergleich österreichische Sterbetafeln, YOB = 1972 (bei Generationentafeln)")
#
# plotValuationTables(mort.AT.census.2001.male, AVOe2005R.male, YOB = 1972, title = "Vergleich österreichische Sterbetafeln")
# plotValuationTables(getCohortTable(AVOe2005R.male, YOB = 1972), getCohortTable(AVOe2005R.male, YOB = 2016), title = "Vergleich österreichische Sterbetafeln")
