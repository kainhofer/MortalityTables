stopifnot(require(methods), require(utils), require(ValuationTables)) # ValuationTable classes; new; Excel reader

#' German Life Tables for (pure) endowments, loaded and unloaded
#'   - DAV1994T: General German risk life table, based on general mortality 1986/88
#'               Available objects:
#'                  o DAV1994T.male (male, loaded)
#'                  o DAV1994T.male.2Ord (male, unloaded)
#'                  o DAV1994T.female (female, loaded)
#'                  o DAV1994T.female.2Ord (female, unloaded)
"Germany_Endowments"

###############################################################################
### DAV 1994T (male, female), 1st-order and general mortality 1986/88
###############################################################################

DAV1994T.data = utils::read.csv(
    system.file("extdata", "Germany_Endowments_DAV1994T.csv",
                package = "ValuationTables"),
    skip = 1);

# DAV1994T (Male, Female), 1st-order and general mortality 1986/88
DAV1994T.male = valuationTable.period(
    name       = "DAV 1994T male, loaded",
    ages       = DAV1994T.data$Alter,
    deathProbs = DAV1994T.data$qbar_x.a)

DAV1994T.male.2Ord = valuationTable.period(
    name       = "DAV 1994T male, unloaded",
    ages       = DAV1994T.data$Alter,
    deathProbs = DAV1994T.data$q_x)

DAV1994T.female = valuationTable.period(
    name       = "DAV 1994T female, loaded",
    ages       = DAV1994T.data$Alter,
    deathProbs = DAV1994T.data$qbar_y.a )

DAV1994T.female.2Ord = valuationTable.period(
    name       = "DAV 1994T female, unloaded",
    ages       = DAV1994T.data$Alter,
    deathProbs = DAV1994T.data$q_y)

rm(DAV1994T.data)

