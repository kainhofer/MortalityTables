stopifnot(require(methods), require(utils), require(ValuationTables)) # ValuationTable classes; new; Excel reader


###############################################################################
### RR67 Rententafel für Männer, 3%
###############################################################################

rr67.data = utils::read.csv(system.file("extdata", "Austria_Annuities_RR67.csv", package="ValuationTables"), skip=2)

RR67 = valuationTable.period(
  name = "ÖVM 59/61 RR67",
  ages = rr67.data$Alter,
  deathProbs = rr67.data$qx
);
rm(rr67.data)

