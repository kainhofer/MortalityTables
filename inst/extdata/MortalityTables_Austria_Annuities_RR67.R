stopifnot(require(methods), require(utils), require(MortalityTables)) # MortalityTable classes; new; Excel reader


###############################################################################
### RR67 Rententafel für Männer, 3%
###############################################################################

rr67.data = utils::read.csv(system.file("extdata", "Austria_Annuities_RR67.csv", package="MortalityTables"), skip=2)

RR67 = mortalityTable.period(
  name = "ÖVM 59/61 RR67",
  ages = rr67.data$Alter,
  deathProbs = rr67.data$qx
);
rm(rr67.data)

