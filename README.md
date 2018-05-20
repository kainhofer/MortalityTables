# MortalityTables
## Author: Reinhold Kainhofer, reinhold@kainhofer.com

R package implementing actuarial mortality tables (period and cohort life tables) 

## About the package

The MortalityTables package provides the `mortalityTable` base class and
some derived classes to handle different types of mortality tables (also 
called life tables), mainly
used for life insurance. Additionally it provides a plot function to compare
multiple life tables either directly using the absolute mortalities in
log-linear plots or using relative mortalities as percentages of a given
reference table.

### Types of Life Tables

Provided types of mortality tables are:

* Base class
    : Class `mortalityTable`
* Period life table
    : Class `mortalityTable.period(ages, deathProbs, ..., baseYear=2000)`
    : Death probabilities observed / predicted for one observation year;
      No dependency on the bith year is assumed.
* Cohort life table using age-specific trends
    : Class `mortalityTable.trendProjection`
    : Death probabilities of a given base year are projected into the future
      using age-specific trends $\lambda_x$. The death probability of an $x$-year old in year
      `baseYear + n` is calculated as:
          $$q_x^{(baseYear+n)} = q_x^{(baseYear)} \cdot e^{-n\cdot\lambda_x}$$
    : Consequently, the death probabilities for a person born in year `YOB` can be calculated as
        $$q_x^{YOB} = q_x^{(base)} \cdot e^{-(YOB+x-baseYear)\cdot \lambda_x}$$
* Cohort life table approximation using age shift
    : Class `mortalityTable.ageShift`
    : Death probabilities for cohort $YOB$ are obtained by using death probabilities
      for cohort $X$ and modifying the technical age with a birth-year dependent shift:
          $$q_x^{YOB} = q_{x+shift(YOB)}^{(base)}$$
<!-- * Observed life table -->
<!--     : Class `mortalityTable.observed` -->
<!--     : Death probabilities observed during several years. The probabilities are -->
<!--       stored as a matrix with observation year and age as dimensions. -->
* Mixed life table
    : Class `mortalityTable.mixed`
    : Arithmetic mean of two life tables with given weights. This approach is
      often used to generate unisex life tables by mixing male and female
      mortalities with given weights (e.g. 70:30 or 40:60)
* Cohort life table using age-specific improvement factors
    : Class `mortalityTable.improvementFactors`
    : Project base life table using age-specific improvement factors.
* Pension table
    : Class `pensionTable`
    : Four states: active, early retirement / invalidity, old-age pension, death (with optional widow)
    : All slots describe the corresponding transition probabilities by a 
    : `mortalityTable`-derived object.

## Loading the MortalityTables package
```
library("MortalityTables")
```

## Provided Data Sets

The package provides several real-life life tables published by census bureaus 
and actuarial associations around the world. You can use the function 
`mortalityTables.list` to list all available datasets (if no argument is given)
or all datasets that match the given pattern (wildcard character is *). You can 
then use `mortalityTables.load` to load either one single data set or all 
datasets that match the pattern.

```
# list all available data sets
mortalityTables.list()

# list all datasets for Austria
mortalityTables.list("Austria_*")

# Load the German annuity table DAV 2004-R
mortalityTables.load("Germany_Annuities_DAV2004R")

# Load all Austrian data sets
mortalityTables.load("Austria_*")
```

## Further information
For further information on how to use the package, see the "Using the MortalityTables Package" vignette.
