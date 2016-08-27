stopifnot(require(methods), require(utils), require(ValuationTables))

###############################################################################
### 2012 IAR/GAM group annuity tables, with improvement factors AA_x
###############################################################################

USA2012IAM.data = utils::read.csv(
    system.file("extdata",
                "USA_Annuities_2012IAM.csv",
                package = "ValuationTables"),
    col.names = c("age", "qxBasic", "qyBasic", "qx", "qy", "G2x", "G2y", "", "", ""),
    skip = 3
)

USA2012IAM.male.basic = valuationTable.period (
    name = "USA 2012 IAM basic (unloaded), male",
    ages = USA2012IAM.data$age,
    deathProbs = USA2012IAM.data$qxBasic)

USA2012IAM.female.basic = valuationTable.period (
    name = "USA 2012 IAM basic (unloaded), female",
    ages = USA2012IAM.data$age,
    deathProbs = USA2012IAM.data$qyBasic)


USA2012IAM.male = valuationTable.improvementFactors (
    name = "USA 2012 IAM, male",
    ages = USA2012IAM.data$age,
    deathProbs = USA2012IAM.data$qx,
    improvement = USA2012IAM.data$G2x)

USA2012IAM.female = valuationTable.improvementFactors (
    name = "USA 2012 IAM, female",
    ages = USA2012IAM.data$age,
    deathProbs = USA2012IAM.data$qy,
    improvement = USA2012IAM.data$G2y)

rm(USA2012IAM.data)

