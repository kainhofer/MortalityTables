stopifnot(require(methods), require(utils), require(MortalityTables))

###############################################################################
### 1994 GAR/GAM group annuity tables, with improvement factors AA_x
###############################################################################

USA1994GAM.data = utils::read.csv(
    system.file("extdata",
                "USA_Annuities_1994GAR.csv",
                package = "MortalityTables"),
    col.names = c("age","qx", "AAx", "qy", "AAy", "qxBasic", "qyBasic"),
    skip = 3)

USA1994GAM.male.basic = mortalityTable.period(
    name = "USA 1994 GAM basic (unloaded), male",
    ages = USA1994GAM.data$age,
    deathProbs = USA1994GAM.data$qxBasic,
    data = list(
        dim = list(sex = "m", collar = "group Mortality", type = "Rententafel", data = "unloaded", year = "1994 GAM")
    )
)

USA1994GAM.female.basic = mortalityTable.period(
    name = "USA 1994 GAM basic (unloaded), female",
    ages = USA1994GAM.data$age,
    deathProbs = USA1994GAM.data$qyBasic,
    data = list(
        dim = list(sex = "m", collar = "group Mortality", type = "Rententafel", data = "unloaded", year = "1994 GAM")
    )
)


USA1994GAR.male = mortalityTable.improvementFactors(
    name = "USA 1994 GAR, male",
    ages = USA1994GAM.data$age,
    deathProbs = USA1994GAM.data$qx,
    improvement = USA1994GAM.data$AAx,
    data = list(
        dim = list(sex = "m", collar = "group Reserving", type = "Rententafel", data = "loaded", year = "1994 GAR")
    )
)

USA1994GAR.female = mortalityTable.improvementFactors(
    name = "USA 1994 GAR, female",
    ages = USA1994GAM.data$age,
    deathProbs = USA1994GAM.data$qy,
    improvement = USA1994GAM.data$AAy,
    data = list(
        dim = list(sex = "m", collar = "group Reserving", type = "Rententafel", data = "loaded", year = "1994 GAR")
    )
)

rm(USA1994GAM.data)

# plot(USA1994GAM.male.basic, USA1994GAR.male, Period = 1994)

