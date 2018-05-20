stopifnot(require(methods), require(utils), require(MortalityTables)) # MortalityTable classes; new; Excel reader

#' ADSt 1924/26, 2 Lives, for (pure) endowments,  unloaded: ADSt2426.2Lives
"Austria_Endowments_ADSt2426_2Lives"

###############################################################################
### ADSt 1924/26 Männer, 2 verbundene Leben
###############################################################################

ADSt2426.2Lives.data = utils::read.csv(
    system.file("extdata", "Austria_Endowments_ADSt2426_2Lives.csv",
                package = "MortalityTables"),
    skip = 1);

ADSt2426.2Lives = mortalityTable.period(
    name       = "ADSt 1924/26 Männer, 2 Leben",
    ages       = ADSt2426.2Lives.data$xx,
    deathProbs = ADSt2426.2Lives.data$qxx,
    data = list(
        dim = list(sex = "m", collar = "2-Leben", type = "Sterbetafel Deutschland", data = "verbundene Leben", year = "1924/26")
    )
)

rm(ADSt2426.2Lives.data)

