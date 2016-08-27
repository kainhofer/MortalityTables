stopifnot(require(methods), require(utils), require(ValuationTables))

###############################################################################
### 1983 Table "a" (individual) and GAM (group annuities), period tables
###############################################################################

USA1983a.data = utils::read.csv(
    system.file("extdata",
                "USA_Annuities_1983a_GAM.csv",
                package = "ValuationTables"),
    col.names = c("age","qx", "qy", "qxG", "qyG"),
    skip = 3)


USA1983a.male = valuationTable.period (
  name = "USA 1983 Table a, male",
  ages = USA1983a.data$age,
  deathProbs = USA1983a.data$qx)

USA1983a.female = valuationTable.period (
  name = "USA 1983 Table a, female",
  ages = USA1983a.data$age,
  deathProbs = USA1983a.data$qy)


USA1983GAM.male = valuationTable.period (
  name = "USA 1983 GAM, male",
  ages = USA1983a.data$age,
  deathProbs = USA1983a.data$qxG)

USA1983GAM.female = valuationTable.period (
  name = "USA 1983 GAM, female",
  ages = USA1983a.data$age,
  deathProbs = USA1983a.data$qyG)

rm(USA1983a.data)

# plotValuationTables(USA1971IAM.male, USA1971IAM.male.projected, YOB = 1971)
# plotValuationTables(USA1971IAM.male, USA1971IAM.male.projected, Period = 1999)
# plotValuationTables(USA1971IAM.male, USA1983a.male, USA1983GAM.male, YOB = 1971)

