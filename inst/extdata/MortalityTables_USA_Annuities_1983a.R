stopifnot(require(methods), require(utils), require(MortalityTables))

###############################################################################
### 1983 Table "a" (individual) and GAM (group annuities), period tables
###############################################################################

USA1983a.data = utils::read.csv(
    system.file("extdata",
                "USA_Annuities_1983a_GAM.csv",
                package = "MortalityTables"),
    col.names = c("age","qx", "qy", "qxG", "qyG"),
    skip = 3)


USA1983a.male = mortalityTable.period (
  name = "USA 1983 Table a, male",
  ages = USA1983a.data$age,
  deathProbs = USA1983a.data$qx)

USA1983a.female = mortalityTable.period (
  name = "USA 1983 Table a, female",
  ages = USA1983a.data$age,
  deathProbs = USA1983a.data$qy)


USA1983GAM.male = mortalityTable.period (
  name = "USA 1983 GAM, male",
  ages = USA1983a.data$age,
  deathProbs = USA1983a.data$qxG)

USA1983GAM.female = mortalityTable.period (
  name = "USA 1983 GAM, female",
  ages = USA1983a.data$age,
  deathProbs = USA1983a.data$qyG)

rm(USA1983a.data)

# plot(USA1971IAM.male, USA1971IAM.male.projected, YOB = 1971)
# plot(USA1971IAM.male, USA1971IAM.male.projected, Period = 1999)
# plot(USA1971IAM.male, USA1983a.male, USA1983GAM.male, YOB = 1971)

