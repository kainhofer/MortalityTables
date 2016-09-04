stopifnot(require(methods), require(utils), require(MortalityTables))


###############################################################################
### 1971 IAM individual annuity table; with optional projection scale B_x
###############################################################################

USA1971IAM.data = utils::read.csv(
    system.file("extdata",
                "USA_Annuities_1971IAM.csv",
                package = "MortalityTables"),
    col.names = c("age","qx", "qy", "B"),
    skip = 3)

USA1971IAM.male = mortalityTable.period (
  name = "USA 1971 IAM, male",
  ages = USA1971IAM.data$age,
  deathProbs = USA1971IAM.data$qx)

USA1971IAM.female = mortalityTable.period (
  name = "USA 1971 IAM, female",
  ages = USA1971IAM.data$age,
  deathProbs = USA1971IAM.data$qy)

USA1971IAM.male.projected = mortalityTable.improvementFactors (
  name = "USA 1971 IAM, male, projected",
  ages = USA1971IAM.data$age,
  baseYear = 1971,
  deathProbs = USA1971IAM.data$qx,
  improvement = USA1971IAM.data$B)

USA1971IAM.female.projected = mortalityTable.improvementFactors (
  name = "USA 1971 IAM, female, projected",
  ages = USA1971IAM.data$age,
  baseYear = 1971,
  deathProbs = USA1971IAM.data$qy,
  improvement = USA1971IAM.data$B)

rm(USA1971IAM.data)

# plot(USA1971IAM.male, USA1971IAM.male.projected, YOB = 1971)
# plot(USA1971IAM.male, USA1971IAM.male.projected, Period = 1999)
