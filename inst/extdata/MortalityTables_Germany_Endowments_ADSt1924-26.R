stopifnot(require(methods), require(utils), require(MortalityTables)) # MortalityTable classes; new; Excel reader

#' German Life Tables for (pure) endowments, loaded and unloaded
#'   - ADSt 1924/26: German population mortality tables
"Germany_Endowments"

######################################################
##  DAV 2008T Aggregat / Smoker / Non-Smoker
######################################################

ADSt192426.data = utils::read.csv(
    system.file("extdata", "Germany_Endowments_ADSt1924-26_M.csv",
                package = "MortalityTables"),
    col.names = c(
        "age", "qx", "Ex"));

ADSt192426.male = mortalityTable.period(
  name = "ADSt 1924/26 male",
  ages = ADSt192426.data$age,
  deathProbs = ADSt192426.data$qx,
  data = list(
      dim = list(sex = "m", collar = "Aggregat", type = "Population", data = "unloaded", year = "1924")
  )
)


rm(ADSt192426.data)


# plot(ADSt192426.male)

