stopifnot(require(methods), require(utils), require(MortalityTables))

###############################################################################
### Annuity 2000 Basic (unloaded) and Mortality (loaded) Tables, PERIOD tables
###############################################################################

USAAnnuity2000.data = utils::read.csv(
    system.file("extdata",
                "USA_Annuities_Annuity2000.csv",
                package = "MortalityTables"),
    col.names = c("age","qxBasic", "qyBasic", "qx", "qy"),
    skip = 4
)


USAAnnuity2000.basic.male = mortalityTable.period (
    name = "USA Annuity 2000 basic, male",
    ages = USAAnnuity2000.data$age,
    deathProbs = USAAnnuity2000.data$qxBasic)

USAAnnuity2000.basic.female = mortalityTable.period (
    name = "USA Annuity 2000 basic, female",
    ages = USAAnnuity2000.data$age,
    deathProbs = USAAnnuity2000.data$qyBasic)


USAAnnuity2000.male = mortalityTable.period (
    name = "USA Annuity 2000 mortality, male",
    ages = USAAnnuity2000.data$age,
    deathProbs = USAAnnuity2000.data$qx)

USAAnnuity2000.female = mortalityTable.period (
    name = "USA Annuity 2000 mortality, female",
    ages = USAAnnuity2000.data$age,
    deathProbs = USAAnnuity2000.data$qy)

rm(USAAnnuity2000.data)

# plot(USAAnnuity2000.basic.male, USAAnnuity2000.male, Period = 2000)


