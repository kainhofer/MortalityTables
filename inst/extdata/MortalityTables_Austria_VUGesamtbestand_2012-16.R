#' @import MortalityTables
NULL

stopifnot(require(methods), require(utils), require(MortalityTables), require(tidyverse), require(reshape2))

############################################################################### #
# Bestandssterbetafel der Versucherungsunternehmen Österreichs, 2012-6       ----
############################################################################### #

VUBestandstafel.Detail.load = function() {
    basedata = utils::read.csv(
        system.file("extdata", "VU_Gesamtbestand_Austria_Detail_2012-16.csv", package = "MortalityTables"),
        # skip = 7,
        encoding = "UTF-8",
        header = TRUE
    )

    basedata.array = basedata %>% as_tibble %>%
        gather(Variable, Value, smooth, raw, Exposure) %>%
        acast(tariff ~ sex ~ premium ~ probability ~ age ~ Variable, value.var = "Value")


    VU.Gesamtbestand.Detail = array(
        data = c(mortalityTable.NA),
        dim = c(dim(basedata.array)[1:4], 2),
        dimnames = c(`names<-`(dimnames(basedata.array)[1:4], c("Tarif", "Geschlecht", "Prämie", "Wahrscheinlichkeit")), list(Typ = c("raw", "smooth"))))

    dmn = dimnames(basedata.array)[1:4]
    q = "qx"
    ages = 0:99
    ages.ch = as.character(ages)
    for (t in dmn[[1]]) {
        for (s in dmn[[2]]) {
            for (p in dmn[[3]]) {
                    VU.Gesamtbestand.Detail[[t, s, p, q, "smooth"]] = mortalityTable.period(
                        name = sprintf("VU Gesamtbestand AT, %s, %s, %s, %s, geglättet", t, s, p, q),
                        ages = ages,
                        deathProbs = basedata.array[t, s, p, q, ages.ch, "smooth"],
                        exposures = basedata.array[t, s, p, q, ages.ch, "Exposure"],
                        data = list(
                            raw = basedata.array[t, s, p, q, ages.ch, "raw"],
                            dim = list(
                                sex = s,
                                year = "(all)",
                                data = "smooth",
                                tarif = t,
                                table = "VU Gesamtbestand AT",
                                zahlart = p,
                                probability = q
                        ))
                    )
                    VU.Gesamtbestand.Detail[[t, s, p, q, "raw"]] = mortalityTable.period(
                        name = sprintf("VU Gesamtbestand AT, %s, %s, %s, %s, roh", t, s, p, q),
                        ages = ages,
                        deathProbs = basedata.array[t, s, p, q, ages.ch, "raw"],
                        exposures = basedata.array[t, s, p, q, ages.ch, "Exposure"],
                        data = list(dim = list(
                            sex = s,
                            year = "(all)",
                            data = "raw",
                            tarif = t,
                            table = "VU Gesamtbestand AT",
                            zahlart = p,
                            probability = q
                        ))
                    )
            }
        }
    }
    q = "sx"
    ages = 0:40
    ages.ch = as.character(ages)
    for (t in dmn[[1]]) {
        for (s in dmn[[2]]) {
            for (p in dmn[[3]]) {
                VU.Gesamtbestand.Detail[[t, s, p, q, "raw"]] = mortalityTable.period(
                    name = sprintf("VU Gesamtbestand AT, %s, %s, %s, %s, roh", t, s, p, q),
                    ages = ages,
                    deathProbs = basedata.array[t, s, p, q, ages.ch, "raw"],
                    exposures = basedata.array[t, s, p, q, ages.ch, "Exposure"],
                    data = list(dim = list(
                        sex = s,
                        year = "(all)",
                        data = "raw",
                        tarif = t,
                        table = "VU Gesamtbestand AT",
                        zahlart = p,
                        probability = q
                    ))
                )
            }
        }
    }
    VU.Gesamtbestand.Detail
    # plotMortalityTables(VU.Gesamtbestand.Detail[,,,"sx","raw"], legend.position = "bottom", log = FALSE)
    # makeQxDataFrame(VU.Gesamtbestand.Detail[,,,"sx",])
    # unlist(VU.Gesamtbestand.Detail[,,,"sx",])
}

VUBestandstafel.qx.load = function() {
    basedata = utils::read.csv(
        system.file("extdata", "VU_Gesamtbestand_Austria_qx_2012-16.csv", package = "MortalityTables"),
        # skip = 7,
        encoding = "UTF-8",
        header = TRUE
    )

    sex = c("m", "f", "u")
    VU.Gesamtbestand = array(
        data = c(mortalityTable.NA),
        dim = c(3),
        dimnames = list(sex = sex)
    )

    for (s in sex) {
        data = filter(basedata, sex == s)
        data = data[order(data$age),]

        VU.Gesamtbestand[[s]] = mortalityTable.period(
            name = paste0("VU-Gesamtbestand 2012-2016, ", recode(s, "m" = "Männer", "f" = "Frauen", "u" = "Unisex")),
            ages = data$age,
            deathProbs = data$smooth,
            baseYear = 2014,
            exposures = data$exposure,
            data = list(
                raw = data$raw,
                dim = list(
                    sex = s,
                    year = "2014",
                    data = "smooth",
                    tarif = "(all)",
                    table = "VU Gesamtbestand AT",
                    probability = "qx",
                    population = "Lebensversicherte",
                    country = "AT",
                    period = "2012-2016"
                )
            )

        )
    }
    VU.Gesamtbestand
}
VUBestandstafel.Storno.load = function() {
    basedata = utils::read.csv(
        system.file("extdata", "VU_Gesamtbestand_Austria_Storno_2012-16.csv", package = "MortalityTables"),
        # skip = 7,
        encoding = "UTF-8",
        header = TRUE
    )

    tarif = c("KLV", "FLV", "Sonstige")
    VU.Gesamtbestand = array(
        data = c(mortalityTable.NA),
        dim = c(3),
        dimnames = list(tarif = tarif)
    )

    for (t in tarif) {
        data = filter(basedata, tarif == t)
        data = data[order(data$age),]

        VU.Gesamtbestand[[t]] = mortalityTable.period(
            name = paste0("VU-Gesamtbestand 2012-2016, Storno ", t),
            ages = data$age,
            deathProbs = data$sx,
            baseYear = 2014,
            exposures = data$exposure,
            data = list(
                dim = list(
                    sex = "u",
                    year = "2014",
                    data = "raw",
                    tarif = t,
                    table = "VU Gesamtbestand AT",
                    probability = "sx",
                    population = "Lebensversicherte",
                    country = "AT",
                    period = "2012-2016"
                )
            )

        )
    }
    VU.Gesamtbestand
}


VU.Gesamtbestand.Detail = VUBestandstafel.Detail.load()
VU.Gesamtbestand = VUBestandstafel.qx.load()
VU.Gesamtbestand.Storno = VUBestandstafel.Storno.load()

# plotMortalityTables(VU.Gesamtbestand)
# plotMortalityTables(VU.Gesamtbestand.Storno, log = FALSE, aes = aes(color = tarif))

rm(VUBestandstafel.Detail.load, VUBestandstafel.qx.load, VUBestandstafel.Storno.load)

# plotMortalityTables(VU.Gesamtbestand["(all)", c("m", "w"),"(all)","qx",])

# mortalityTables.list()
# mortalityTables.load("Austria_VU*")
