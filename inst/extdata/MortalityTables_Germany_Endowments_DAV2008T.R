stopifnot(require(methods), require(utils), require(MortalityTables)) # MortalityTable classes; new; Excel reader

#' German Life Tables for (pure) endowments, loaded and unloaded
#'   - DAV2008T: General German risk life table published by the DAV 2008
#'               Available objects:
#'                  o DAV2008T.male  (aggregate male, loaded)
#'                  o DAV2008T.male.2Ord (aggregate male, unloaded)
#'                  o DAV2008T.female (aggregate female, loaded)
#'                  o DAV2008T.female.2Ord (aggregate female, unloaded)
#'                  o DAV2008T.male.smoker (male smoker, loaded)
#'                  o DAV2008T.male.smoker.2Ord (male smoker, unloaded)
#'                  o DAV2008T.female.smoker (female smoker, loaded)
#'                  o DAV2008T.female.smoker.2Ord (female smoker, unloaded)
#'                  o DAV2008T.male.nonsmoker (male non-smoker, loaded)
#'                  o DAV2008T.male.nonsmoker.2Ord (male non-smoker, unloaded)
#'                  o DAV2008T.female.nonsmoker (female non-smoker, loaded)
#'                  o DAV2008T.female.nonsmoker.2Ord (female non-smoker, unloaded)
"Germany_Endowments"

######################################################
##  DAV 2008T Aggregat / Smoker / Non-Smoker
######################################################

DAV2008T.data = utils::read.csv(
    system.file("extdata", "Germany_Endowments_DAV2008T.csv",
                package = "MortalityTables"),
    col.names = c(
        "age", "", "", "",
                  "qx2", "qx2NR", "qx2R",  # male 2nd order
                  "qx1", "qx1NR", "qx1R",  # male 1st order
                  "", "", "", "",
                  "qy2", "qy2NR", "qy2R",  # female 2nd order
                  "qy1", "qy1NR", "qy1R"), # female 1st order
    skip = 4);

### DAV 2008T Aggregat (smoker+non-smoker combined)
DAV2008T.male = mortalityTable.period(
  name = "DAV 2008T male, loaded",
  ages = DAV2008T.data$age,
  deathProbs = DAV2008T.data$qx1,
  data = list(
      dim = list(sex = "m", collar = "Aggregat", type = "Risikotafel", data = "loaded", year = "DAV 2008T")
  )
)

DAV2008T.male.2Ord = mortalityTable.period(
  name = "DAV 2008T male, unloaded",
  ages = DAV2008T.data$age,
  deathProbs = DAV2008T.data$qx2,
  data = list(
      dim = list(sex = "m", collar = "Aggregat", type = "Risikotafel", data = "unloaded", year = "DAV 2008T")
  )
)

DAV2008T.female = mortalityTable.period(
  name = "DAV 2008T female, loaded",
  ages = DAV2008T.data$age,
  deathProbs = DAV2008T.data$qy1,
  data = list(
      dim = list(sex = "w", collar = "Aggregat", type = "Risikotafel", data = "loaded", year = "DAV 2008T")
  )
)

DAV2008T.female.2Ord = mortalityTable.period(
  name = "DAV 2008T female, unloaded",
  ages = DAV2008T.data$age,
  deathProbs = DAV2008T.data$qy2,
  data = list(
      dim = list(sex = "w", collar = "Aggregat", type = "Risikotafel", data = "unloaded", year = "DAV 2008T")
  )
)

### DAV 2008T Smoker

DAV2008T.male.smoker = mortalityTable.period(
    name = "DAV 2008T male smoker, loaded",
    ages = DAV2008T.data$age,
    deathProbs = DAV2008T.data$qx1R,
    data = list(
        dim = list(sex = "m", collar = "Raucher", type = "Risikotafel", data = "loaded", year = "DAV 2008T")
    )
)

DAV2008T.male.smoker.2Ord = mortalityTable.period(
    name = "DAV 2008T male smoker, unloaded",
    ages = DAV2008T.data$age,
    deathProbs = DAV2008T.data$qx2R,
    data = list(
        dim = list(sex = "m", collar = "Raucher", type = "Risikotafel", data = "unloaded", year = "DAV 2008T")
    )
)

DAV2008T.female.smoker = mortalityTable.period(
    name = "DAV 2008T female smoker, loaded",
    ages = DAV2008T.data$age,
    deathProbs = DAV2008T.data$qy1R,
    data = list(
        dim = list(sex = "w", collar = "Raucher", type = "Risikotafel", data = "loaded", year = "DAV 2008T")
    )
)

DAV2008T.female.smoker.2Ord = mortalityTable.period(
    name = "DAV 2008T female smoker, unloaded",
    ages = DAV2008T.data$age,
    deathProbs = DAV2008T.data$qy2R,
    data = list(
        dim = list(sex = "w", collar = "Raucher", type = "Risikotafel", data = "unloaded", year = "DAV 2008T")
    )
)


### DAV 2008T Non-Smoker
DAV2008T.male.nonsmoker = mortalityTable.period(
  name = "DAV 2008T male non-smoker, loaded",
  ages = DAV2008T.data$age,
  deathProbs = DAV2008T.data$qx1NR,
  data = list(
      dim = list(sex = "m", collar = "Nichtraucher", type = "Risikotafel", data = "loaded", year = "DAV 2008T")
  )
)

DAV2008T.male.nonsmoker.2Ord = mortalityTable.period(
  name = "DAV 2008T male non-smoker, unloaded",
  ages = DAV2008T.data$age,
  deathProbs = DAV2008T.data$qx2NR,
  data = list(
      dim = list(sex = "m", collar = "Nichtraucher", type = "Risikotafel", data = "unloaded", year = "DAV 2008T")
  )
)


DAV2008T.female.nonsmoker = mortalityTable.period(
  name = "DAV 2008T female non-smoker, loaded",
  ages = DAV2008T.data$age,
  deathProbs = DAV2008T.data$qy1NR,
  data = list(
      dim = list(sex = "w", collar = "Nichtraucher", type = "Risikotafel", data = "loaded", year = "DAV 2008T")
  )
)

DAV2008T.female.nonsmoker.2Ord = mortalityTable.period(
  name = "DAV 2008T female non-smoker, unloaded",
  ages = DAV2008T.data$age,
  deathProbs = DAV2008T.data$qy2NR,
  data = list(
      dim = list(sex = "w", collar = "Nichtraucher", type = "Risikotafel", data = "unloaded", year = "DAV 2008T")
  )
)

rm(DAV2008T.data)


# plot(DAV1994T.male, DAV2008T.male, DAV2008T.male.smoker, DAV2008T.male.nonsmoker, DAV2008T.male.2Ord, DAV2008T.male.smoker.2Ord, DAV2008T.male.nonsmoker.2Ord, legend.position = c(1, 0), title = "DAV 2008T Vergleich Raucher-Nichtraucher")
#
# plot(DAV1994T.male, DAV2008T.male.2Ord, DAV2008T.male.smoker.2Ord, DAV2008T.male.nonsmoker.2Ord, legend.position = c(1, 0), title = "DAV 2008T Vergleich Raucher-Nichtraucher")
#
# plot(DAV1994T.male, DAV2008T.male.2Ord, DAV2008T.male.smoker.2Ord, DAV2008T.male.nonsmoker.2Ord, legend.position = c(1, 0), title = "DAV 2008T Vergleich Raucher-Nichtraucher", reference = DAV2008T.male.2Ord, ylim=c(.5,2))

