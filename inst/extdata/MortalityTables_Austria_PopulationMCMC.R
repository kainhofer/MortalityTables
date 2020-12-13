stopifnot(require(methods), require(utils), require(MortalityTables), require(tidyverse), require(reshape2)) # MortalityTable classes; new; Excel reader


###############################################################################
### Gesamtbevölkerung Österreich: Bevölkerungsprognose bis 2080 (mittleres Szenario)
### Datenquelle: Statistik Austria
###############################################################################

mort.AT.MCMC.load = function() {
    data = utils::read.csv(system.file("extdata", "Austria_Population_MCMC2018.csv", package = "MortalityTables"), skip = 5, encoding = "UTF-8", check.names = FALSE);
    data.array = data %>% as_tibble %>%
        gather(Variable, Value, -Parameter, -Alter) %>%
        separate(Variable, into = c("Geschlecht", "Jahr"), sep = ", ") %>%
        filter(Jahr == "1980") %>%
        mutate(Jahr = NULL) %>%
        acast(Alter ~ Geschlecht ~ Parameter, value.var = "Value")

    exp(data.array[,,"alpha"])/2
    mort.AT.MCMC = array(
        data = c(mortalityTable.NA),
        dim = c(3),
        dimnames = list(Geschlecht = c("m", "w", "u"))
    )

    MCMC.trend.damping = function(t) { 200 * atan(t / 200) }

    # Parameter für Whittaker-Smoothing:
    d = 2
    lambda = 10
    # TODO: Eta einbauen
    mort.AT.MCMC[["m"]] =  mortalityTable.trendProjection(
        name = "Österreich MCMC Männer",
        ages = as.integer(dimnames(data.array)[[1]]),
        baseYear = 2008,
        deathProbs = exp(whittaker(data.array[,"Mann","alpha"], lambda = lambda, d = d))/2,
        trend = whittaker(-data.array[,"Mann","beta"], lambda = lambda, d = d),
        dampingFunction = MCMC.trend.damping,
        data = list(
            dim = list(sex = "m", collar = "Gesamtbevölkerung", type = "MCMC-Fit 1980-2017", data = "MCMC", year = "1980-2017", Tafel = "MCMC-Zerlegung Bevölkerungssterblichkeit")
        )
    ) %>%
        mT.fitExtrapolationLaw(law = "HP2", method = "LF2", fit = 80:98, extrapolate = 90:120, fadeIn = 90:99) %>%
        mT.extrapolateTrendExp(idx = 92, up = TRUE)

    mort.AT.MCMC[["w"]] =  mortalityTable.trendProjection(
        name = "Österreich MCMC Frauen",
        ages = as.integer(dimnames(data.array)[[1]]),
        baseYear = 2008,
        deathProbs = exp(whittaker(data.array[,"Frau","alpha"], lambda = lambda, d = d))/2,
        trend = whittaker(-data.array[,"Frau","beta"], lambda = lambda, d = d),
        dampingFunction = MCMC.trend.damping,
        data = list(
            dim = list(sex = "w", collar = "Gesamtbevölkerung", type = "MCMC-Fit 1980-2017", data = "MCMC", year = "1980-2017", Tafel = "MCMC-Zerlegung Bevölkerungssterblichkeit")
        )
    ) %>%
        mT.fitExtrapolationLaw(law = "HP2", method = "LF2", fit = 80:98, extrapolate = 90:120, fadeIn = 90:99) %>%
        mT.extrapolateTrendExp(idx = 94, up = TRUE)

    mort.AT.MCMC[["u"]] =  mortalityTable.trendProjection(
        name = "Österreich MCMC Unisex",
        ages = as.integer(dimnames(data.array)[[1]]),
        baseYear = 2008,
        deathProbs = exp(whittaker(data.array[,"Unisex","alpha"], lambda = lambda, d = d))/2,
        trend = whittaker(-data.array[,"Unisex","beta"], lambda = lambda, d = d),
        dampingFunction = MCMC.trend.damping,
        data = list(
            dim = list(sex = "u", collar = "Gesamtbevölkerung", type = "MCMC-Fit 1980-2017", data = "MCMC", year = "1980-2017", Tafel = "MCMC-Zerlegung Bevölkerungssterblichkeit")
        )
    ) %>%
        mT.fitExtrapolationLaw(law = "HP2", method = "LF2",fit = 80:98, extrapolate = 90:120, fadeIn = 90:99) %>%
        mT.extrapolateTrendExp(idx = 98, up = TRUE)

    mort.AT.MCMC
}



mort.AT.MCMC = mort.AT.MCMC.load()


rm(mort.AT.MCMC.load)

###############################################################################

# mortalityTables.load("Austria*")
# plotMortalityTables(mort.AT.MCMC, Period = 2008)
# plotMortalityTrend(mort.AT.MCMC, Period = 2008)
