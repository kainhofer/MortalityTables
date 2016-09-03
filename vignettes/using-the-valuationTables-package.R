## ----message=FALSE-------------------------------------------------------
library("ValuationTables")

## ------------------------------------------------------------------------
# list all available data sets
valuationTables.list()

# list all datasets for Austria
valuationTables.list("Austria_*")

# Load the German annuity table DAV 2004-R
valuationTables.load("Germany_Annuities_DAV2004R")

# Load all Austrian data sets
valuationTables.load("Austria_*", wildcard=TRUE)

## ------------------------------------------------------------------------
# Log-linear plot comparing some Austrian census tables
plot(mort.AT.census.1951.male, mort.AT.census.1991.male, 
     mort.AT.census.2001.male, mort.AT.census.2011.male, 
     legend.position=c(1,0))

# Relative death probabilities in percentage of the latest census
plot(mort.AT.census.1951.male, mort.AT.census.1991.male, 
     mort.AT.census.2001.male, 
     reference = mort.AT.census.2011.male, legend.position=c(1,0.75), ylim=c(0,4))

## ------------------------------------------------------------------------
# Comparison of two Austrian annuity tables for birth year 1977
plot(AVÖ1996R.male, AVOe2005R.male, YOB=1977, title="Comparison for YOB=1977")

# Comparison of two Austrian annuity tables for observation year 2020
plot(AVÖ1996R.male, AVOe2005R.male, Period=2020, title="Comparison for observation year 2020")


## ----message=FALSE-------------------------------------------------------
valuationTables.load("Austria_Annuities")
# Get the cohort death probabilities for Austrian Annuitants born in 1977:
qx.coh1977 = deathProbabilities(AVOe2005R.male, YOB=1977)

# Get the period death probabilities for Austrian Annuitants observed in the year 2020:
qx.per2020 = periodDeathProbabilities(AVOe2005R.male, Period=2020)

## ------------------------------------------------------------------------
# Get the cohort death probabilities for Austrian Annuitants born in 1977 as a valuationTable.period object:
table.coh1977 = getCohortTable(AVOe2005R.male, YOB=1977)

# Get the period death probabilities for Austrian Annuitants observed in the year 2020:
table.per2020 = getPeriodTable(AVOe2005R.male, Period=2020)

# Compare those two in a plot:
plot(table.coh1977, table.per2020, title="Comparison of cohort 1977 with Period 2020", legend.position=c(1,0))


## ------------------------------------------------------------------------
lt = valuationTable.period(name="Sample period lifetable", ages=1:99, deathProbs=exp(-(99:1)/10))
plot(lt, title="Simple log-linear period mortality table")
deathProbabilities(lt)


## ------------------------------------------------------------------------
b=AVOe2005R.female 
b@name = "Modified Copy"
# only b is modified, not the original table
b@modification = function(qx) pmax(qx, 0.01)  
plot(AVOe2005R.female, b, YOB=2000)

## ------------------------------------------------------------------------
lt.mod = valuationTable.period(name="Sample modified lifetable (lower bound of 3%)", ages=1:99, deathProbs=exp(-(99:1)/10), modification=function (qx) pmax(0.03, qx))
plot(lt, lt.mod, title="Original and modified table")

## ------------------------------------------------------------------------
AVOe2005R.female.mod = setModification(AVOe2005R.female, modification=function (qx) pmax(0.03, qx));
# Make sure the modified table has a new name, otherwise plots might break
AVOe2005R.female.mod@name = "Modified table (lower bound of 3%)"
plot(AVOe2005R.female, AVOe2005R.female.mod, title="Original and modified table")

