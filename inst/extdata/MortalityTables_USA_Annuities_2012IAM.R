stopifnot(require(methods), require(utils), require(MortalityTables))

###############################################################################
### 2012 IAR/GAM group annuity tables, with improvement factors AA_x
###############################################################################

USA2012IAM.data = utils::read.csv(
    system.file("extdata",
                "USA_Annuities_2012IAM.csv",
                package = "MortalityTables"),
    col.names = c("age", "qxBasic", "qyBasic", "qx", "qy", "G2x", "G2y", "", "", ""),
    skip = 3
)

USA2012IAM.male.basic = mortalityTable.period(
    name = "USA 2012 IAM basic (unloaded), male",
    ages = USA2012IAM.data$age,
    deathProbs = USA2012IAM.data$qxBasic,
    data = list(
        dim = list(sex = "m", collar = "Mortality", type = "Rententafel", data = "unloaded", year = "2012 IAM")
    )
)

USA2012IAM.female.basic = mortalityTable.period(
    name = "USA 2012 IAM basic (unloaded), female",
    ages = USA2012IAM.data$age,
    deathProbs = USA2012IAM.data$qyBasic,
    data = list(
        dim = list(sex = "w", collar = "Mortality", type = "Rententafel", data = "unloaded", year = "2012 IAM")
    )
)


USA2012IAM.male = mortalityTable.improvementFactors(
    name = "USA 2012 IAM, male",
    ages = USA2012IAM.data$age,
    deathProbs = USA2012IAM.data$qx,
    improvement = USA2012IAM.data$G2x,
    data = list(
        dim = list(sex = "m", collar = "Mortality", type = "Rententafel", data = "loaded", year = "2012 IAM")
    )
)

USA2012IAM.female = mortalityTable.improvementFactors(
    name = "USA 2012 IAM, female",
    ages = USA2012IAM.data$age,
    deathProbs = USA2012IAM.data$qy,
    improvement = USA2012IAM.data$G2y,
    data = list(
        dim = list(sex = "w", collar = "Mortality", type = "Rententafel", data = "loaded", year = "2012 IAM")
    )
)

rm(USA2012IAM.data)

