## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(tidyverse.quiet = TRUE)

## ----message=FALSE------------------------------------------------------------
library("MortalityTables")

## -----------------------------------------------------------------------------
# list all available data sets
mortalityTables.list()

# list all datasets for Austria
mortalityTables.list("Austria_*")

# Load the German annuity table DAV 2004-R
mortalityTables.load("Germany_Annuities_DAV2004R")

# Load all Austrian annuity data sets
mortalityTables.load("Austria_Annuities*")
mortalityTables.load("Austria_Census")

## -----------------------------------------------------------------------------
# Log-linear plot comparing some Austrian census tables
plot(mort.AT.census.1951.male, mort.AT.census.1991.male, 
     mort.AT.census.2001.male, mort.AT.census.2011.male, 
     legend.position = c(1,0))

# Relative death probabilities in percentage of the latest census
plot(mort.AT.census.1951.male, mort.AT.census.1991.male, 
     mort.AT.census.2001.male, 
     reference = mort.AT.census.2011.male, legend.position = c(1,0.75), ylim = c(0,4))

## -----------------------------------------------------------------------------
# Comparison of two Austrian annuity tables for birth year 1977
plot(AVOe1996R.male, AVOe2005R.male, YOB = 1977, title = "Comparison for YOB=1977")

# Comparison of two Austrian annuity tables for observation year 2020
plot(AVOe1996R.male, AVOe2005R.male, Period = 2020, title = "Comparison for observation year 2020")


## ----message=FALSE------------------------------------------------------------
mortalityTables.load("Austria_Annuities")
# Get the cohort death probabilities for Austrian Annuitants born in 1977:
qx.coh1977 = deathProbabilities(AVOe2005R.male, YOB = 1977)

# Get the period death probabilities for Austrian Annuitants observed in the year 2020:
qx.per2020 = periodDeathProbabilities(AVOe2005R.male, Period = 2020)

## -----------------------------------------------------------------------------
# Get the cohort death probabilities for Austrian Annuitants born in 1977 as a mortalityTable.period object:
table.coh1977 = getCohortTable(AVOe2005R.male, YOB = 1977)

# Get the period death probabilities for Austrian Annuitants observed in the year 2020:
table.per2020 = getPeriodTable(AVOe2005R.male, Period = 2020)

# Compare those two in a plot:
plot(table.coh1977, table.per2020, title = "Comparison of cohort 1977 with Period 2020", legend.position = c(1,0))


## -----------------------------------------------------------------------------
lt = mortalityTable.period(name = "Sample period lifetable", ages = 1:99, deathProbs = exp(-(99:1)/10))
plot(lt, title = "Simple log-linear period mortality table")
deathProbabilities(lt)


## -----------------------------------------------------------------------------
atPlus2 = mortalityTable.trendProjection(
    name = "Austrian Census Males 2011, 2% yearly trend",
    baseYear = 2011,
    deathProbs = deathProbabilities(mort.AT.census.2011.male),
    ages = ages(mort.AT.census.2011.male),
    trend = rep(0.02, length(ages(mort.AT.census.2011.male)))
)

## -----------------------------------------------------------------------------
atPlus2.damp = mortalityTable.trendProjection(
    name = "Austrian M '11, 2% yearly, damping until 2111",
    baseYear = 2011,
    deathProbs = deathProbabilities(mort.AT.census.2011.male),
    ages = ages(mort.AT.census.2011.male),
    trend = rep(0.02, length(ages(mort.AT.census.2011.male))),
    # damping function: 2011: full effect, linear reduction until yearly trend=0 in 2111:
    # 2011: 100%, 2012: 99%, 2013: 98% => For 2013 we have a cumulative trend 
    # of 297% instead of 300% for three full yearly trends!
    dampingFunction = function(n) { n - n * (n + 1) / 2 / 100 }
)

plot(mort.AT.census.2011.male, atPlus2, atPlus2.damp, YOB = 2011, legend.position = c(0.8,0.75))

## -----------------------------------------------------------------------------
atPlus2.damp2 = mortalityTable.trendProjection(
    name = "Austrian M '11, 2% yearly, 1% long-term",
    baseYear = 2011,
    deathProbs = deathProbabilities(mort.AT.census.2011.male),
    ages = ages(mort.AT.census.2011.male),
    trend = rep(0.02, length(ages(mort.AT.census.2011.male))),
    trend2 = rep(0.01, length(ages(mort.AT.census.2011.male))),
    # damping function interpolates between the two trends: 
    # until 2021 trend 1, from 2031 trend 2, linearly beteen
    dampingFunction = function(year) { 
        if (year <= 2021) 1
        else if (year > 2031) 14.5/(year - 2011)
        else 1 - (year - 2021)*(year - 2021 + 1) / 20 / (year - 2011)
    }
)

plot(mort.AT.census.2011.male, atPlus2, atPlus2.damp, atPlus2.damp2, YOB = 2011, legend.position = c(0.8,0.75))

## -----------------------------------------------------------------------------
baseTableShift = getCohortTable(atPlus2, YOB = 2011);
baseTableShift@name = "Base table of the shift (YOB 2011)"

atShifted = mortalityTable.ageShift(
    name = "Approximation with age shift",
    baseYear = 2011,
    deathProbs = deathProbabilities(baseTableShift),
    ages = ages(baseTableShift),
    ageShifts = data.frame(
        shifts = c(
            rep( 0, 3), 
            rep(-1, 3), 
            rep(-2, 3), 
            rep(-3, 3), 
            rep(-4, 3), 
            rep(-5, 3), 
            rep(-6, 3)
        ),
        row.names = 2011:2031
    )
)

ageShift(atShifted, YOB = 2021)

plot(baseTableShift, atPlus2, atShifted, YOB = 2021, legend.position = c(0.8,0.75))

## -----------------------------------------------------------------------------
b = AVOe2005R.female 
b@name = "Modified Copy"
# only b is modified, not the original table
b@modification = function(qx) pmax(qx, 0.01)  
plot(AVOe2005R.female, b, YOB = 2000)

## -----------------------------------------------------------------------------
AVOe2005R.female.sec = setLoading(AVOe2005R.female, loading = 0.1);
# Make sure the modified table has a new name, otherwise plots might break
AVOe2005R.female.sec@name = "Table with 10% loading"
plot(AVOe2005R.female, AVOe2005R.female.sec, title = "Original and modified table")

## -----------------------------------------------------------------------------
AVOe2005R.female.mod = setModification(AVOe2005R.female, modification = function(qx) pmax(0.03, qx));
# Make sure the modified table has a new name, otherwise plots might break
AVOe2005R.female.mod@name = "Modified table (lower bound of 3%)"
plot(AVOe2005R.female, AVOe2005R.female.mod, title = "Original and modified table")

## ----AustrianPopulation.RawData-----------------------------------------------
library(tidyverse)
data("PopulationData.AT2017", package = "MortalityTables")
PopulationData.AT2017.raw = PopulationData.AT2017 %>%
  select(age, exposure.total, deaths.total) %>%
  mutate(qraw = deaths.total / (exposure.total + deaths.total/2))

## ----AustrianPopulationTableRaw-----------------------------------------------
PopulationTable.AT2017 = mortalityTable.period(
  name = "Austrian Population Mortality 2017 (raw)", 
  baseYear = 2017,
  deathProbs = PopulationData.AT2017.raw$qraw,
  ages = PopulationData.AT2017.raw$age,
  exposures = PopulationData.AT2017.raw$exposure.total,
  data = list(
    deaths = PopulationData.AT2017.raw$deaths.total,
    dim = list(sex = "u", collar = "Population", type = "raw", year = "2017")
  )
)
plotMortalityTables(PopulationTable.AT2017, title = "Austrian population mortality (raw), 2017")

## ----AustrianPopulationTableSmooth--------------------------------------------
PopulationTable.AT2017.smooth = PopulationTable.AT2017 %>%
  whittaker.mortalityTable(lambda = 1/10, d = 2, name.postfix = ", Whittaker") %>%
  mT.setDimInfo(type = "smoothed")
plotMortalityTables(PopulationTable.AT2017, PopulationTable.AT2017.smooth, title = "Austrian population mortality (raw and smoothed), 2017")  +
  aes(colour = type)

## ----AustrianPopulationTableCut100--------------------------------------------
PopulationData.AT2017.raw %>% filter(age > 90)
PopulationTable.AT2017.cut = PopulationTable.AT2017.smooth %>%
  mT.fillAges(0:99) %>%
  mT.setName("Austrian Population Mortality 2017, Whittaker-smoothed and cut at age 99")

## ----AustrianPopulationTableExtrapolated--------------------------------------
PopulationTable.AT2017.ex = PopulationTable.AT2017.smooth %>%
  mT.fitExtrapolationLaw(law = "HP2", fit = 75:99, extrapolate = 80:120, fadeIn = 80:95) %>%
  mT.setDimInfo(type = "smoothed and extrapolated")
plotMortalityTables(PopulationTable.AT2017, PopulationTable.AT2017.smooth, PopulationTable.AT2017.ex, title = "Austrian population mortality (raw and smoothed), 2017")  +
  aes(colour = type)

## ----AustrianPopulationTableFitComparison-------------------------------------
plotMortalityTables(
  PopulationTable.AT2017, 
  PopulationTable.AT2017.smooth %>%
    mT.fitExtrapolationLaw(law = "HP2", fit = 75:99, extrapolate = 80:120, fadeIn = 80:95) %>%
    mT.setDimInfo(type = "Extrapolation: HP2, Fit 75--99"),
  
  PopulationTable.AT2017.smooth %>%
    mT.fitExtrapolationLaw(law = "HP2", fit = 75:85, extrapolate = 80:120, fadeIn = 80:95) %>%
    mT.setDimInfo(type = "Extrapolation: HP, Fit 75--85"),
  
  PopulationTable.AT2017.smooth %>%
    mT.fitExtrapolationLaw(law = "HP2", fit = 90:110, extrapolate = 80:120, fadeIn = 90:100) %>%
    mT.setDimInfo(type = "Extrapolation: HP2, Fit 90--110"),
  
  title = "Examples of different fitting ranges for extrapolation")  +
  aes(colour = type)

## ----AustrianPopulationTableFitFunctionComparison-----------------------------
plotMortalityTables(
  PopulationTable.AT2017, 
  PopulationTable.AT2017.smooth %>%
    mT.fitExtrapolationLaw(law = "HP2", fit = 75:99, extrapolate = 80:120, fadeIn = 80:95) %>%
    mT.setDimInfo(type = "HP2"),
  PopulationTable.AT2017.smooth %>%
    mT.fitExtrapolationLaw(law = "thiele", fit = 75:99, extrapolate = 80:120, fadeIn = 80:95) %>%
    mT.setDimInfo(type = "thiele"),
  PopulationTable.AT2017.smooth %>%
    mT.fitExtrapolationLaw(law = "ggompertz", fit = 75:99, extrapolate = 80:120, fadeIn = 80:95) %>%
    mT.setDimInfo(type = "ggompertz"),
  PopulationTable.AT2017.smooth %>%
    mT.fitExtrapolationLaw(law = "carriere1", fit = 75:99, extrapolate = 80:120, fadeIn = 80:95) %>%
    mT.setDimInfo(type = "carriere1"),

  title = "Examples of different fitting functions for extrapolation (fit 75--99)", 
  ages = 75:120, legend.position = "bottom", legend.key.width = unit(15, "mm"))  +
  aes(colour = type) + labs(colour = "Mortality Law")

## ----AustrianPopulationTableTrendForecast-------------------------------------
mortalityTables.load("Austria_PopulationForecast")
plotMortalityTrend(mort.AT.forecast, title = "Forecast trend (medium scenario) by Statistik Austria")

## ----AustrianPopulationTableTrend---------------------------------------------
PopulationTable.AT2017.trend = PopulationTable.AT2017.ex %>%
  mT.addTrend(mort.AT.forecast$m@trend, trendages = ages(mort.AT.forecast$m)) %>%
  mT.setDimInfo(type = "smoothed, extrapolated, trend")

PopulationTable.AT2017.trend.ex = PopulationTable.AT2017.trend %>%
  mT.extrapolateTrendExp(95) %>%
  mT.setDimInfo(type = "smoothed, extrapolated, trend extrapolated")

plotMortalityTrend(PopulationTable.AT2017.trend, PopulationTable.AT2017.trend.ex,
                   title = "Extrapolating the trend via Exponential function") +
  aes(color = type)



plotMortalityTables(PopulationTable.AT2017, PopulationTable.AT2017.smooth, PopulationTable.AT2017.ex, PopulationTable.AT2017.trend.ex, YOB = 1980, title = "Austrian population mortality (Period 2017 vs. Generation 1980)", legend.position = c(0.01, 0.99), legend.justification = c(0,1))  +
  aes(colour = type)


## ----AustrianPopulationTableFinal---------------------------------------------
# Lots of non-essential or support information is stored inside the table's data field:
PopulationTable.AT2017.trend.ex@data$whittaker

# Clean up the table (remove all non-essential data, but do not modify results)
PopulationTable.AT2017.Cohort.FINAL = PopulationTable.AT2017.trend.ex %>%
  mT.cleanup() %>%
  mT.round(digits = 6) %>%
  mT.setName("Austrian Population Mortality, Period 2017 with trend projection")


## ----AustrianPopulationTableScaled--------------------------------------------
TableForProduct = PopulationTable.AT2017.Cohort.FINAL %>%
  mT.scaleProbs(factor = 1.25, name.postfix = "10% security added")

plotMortalityTables(TableForProduct, PopulationTable.AT2017.Cohort.FINAL, 
                    title = "Adding a security loading of 25%", Period = 2017, legend.position = "bottom")

