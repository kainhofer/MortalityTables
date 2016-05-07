#' Provide life table classes for life insurance purposes
#'
#' @import openxlsx
#' @import methods
#' @import ggplot2
#'
"_PACKAGE"


#' Class valuationTable
#'
#' Class \code{valuationTable} is the (virtual) base class for all valuation
#' tables. It contains the name and some general values applying to all
#' types of tables, but does not contain any data itself. Use a child class
#' to create actual valuation tables.
#'
#' @slot name     The human-readable name of the valuation table
#' @slot baseYear The base year of the valuation table (e.g. for tables with trend projection)
#' @slot modification A function that will be called with the final death probabilities
#'        to give the user a way to modify the final probabilities
#' @slot loading  Additional security loading on the resulting table (single numeric
#'        value, e.g. 0.05 adds 5% security margin to the probabilities)
#'
#' @name valuationTable-class
#' @rdname valuationTable-class
#' @export valuationTable
#' @exportClass valuationTable
valuationTable=setClass(
  "valuationTable",
  slots=list(name="character", baseYear="numeric", loading="numeric", modification="function"),
  prototype=list(name="Actuarial Valuation Table", baseYear=2000, loading=0, modification=identity),
  contains="VIRTUAL"
);


#' A period life table, giving death probabilities for each age, up to
#' maximum age omega. Optionally apply selection factors to the probabilities
#'
#' @slot ages       The ages corresponding to the entries of the deathProbs
#' @slot deathProbs The one-year death probabilities for the ages
#'
#' @export valuationTable_period
#' @exportClass valuationTable_period
valuationTable_period=setClass(
  "valuationTable_period",
  slots=list(ages="numeric", deathProbs="numeric"),
  prototype=list(ages=eval(0:120), deathProbs=rep(1,120)),
  contains="valuationTable"
);


#' A cohort life table, obtained by age-shifting from a given base table (death probabilities
# for a base birth year)
#'
#' @slot ageShifts  A \code{data.frame} with columns \code{YOB} and \code{shifts} giving the age shifts for each birth year
#'
#' @export valuationTable_ageShift
#' @exportClass valuationTable_ageShift
valuationTable_ageShift=setClass(
  "valuationTable_ageShift",
  slots=list(ageShifts="data.frame"),
  prototype=list(ageShifts=data.frame(YOB=c(), shifts=c())),
  contains="valuationTable_period"
);

#' A cohort life table, obtained by a trend projection from a given base table
#' (PODs for a given observation year). Typically, the trend is obtained by
#' the Lee-Carter method or some other trend estimation.
#' The dampingFunction can be used to modify the cumulative years (e.g. G(tau+x) instead of tau+x)
#' If trend2 is given, the G(tau+x) gives the weight of the first trend, 1-G(tau+x) the weight of the second trend
#'
#' @slot baseYear The base year of the trend projection (\code{baseTable} describes the death probabilities in this year)
#' @slot trend    The yearly improvements of the log-death probabilities (per age)
#' @slot dampingFunction A possible damping of the trend. This is a function \code{damping(delta_years)} that gets a vector of years from the baseYear and should return the dampened values.
#' @slot trend2   The alternate trend. If given, the damping function interpolates between \code{trend} and \code{trend2}, otherwise the dumping function simply modifies the coefficients of \code{trend}.
#'
#' @export valuationTable_ageShift
#' @exportClass valuationTable_ageShift
valuationTable_trendProjection=setClass(
  "valuationTable_trendProjection",
  slots=list(baseYear="numeric", trend="numeric", dampingFunction="function", trend2="numeric"),
  prototype=list(baseYear=1980, trend=rep(0,120), dampingFunction=identity, trend2=0),
  contains="valuationTable_period"
);

#' A cohort life table, obtained by an improvment factor projection
#' from a given base table (PODs for a given observation year).
#'
#' @slot baseYear    The base year for the improvements (\code{baseTable} describes the death probabilities in this year)
#' @slot improvement Yearly improvement factors per age
#'
#' @export valuationTable_improvementFactors
#' @exportClass valuationTable_improvementFactors
valuationTable_improvementFactors=setClass(
  "valuationTable_improvementFactors",
  slots=list(baseYear="numeric", improvement="numeric"),
  prototype=list(baseYear=2012, improvement=rep(0,120)),
  contains="valuationTable_period"
);

#' A cohort life table described by actual observations (data frame of PODs
#' per year and age)
#'
#' @slot data    The observations
#'
#' @export valuationTable_observed
#' @exportClass valuationTable_observed
valuationTable_observed=setClass(
  "valuationTable_observed",
  slots=list(data="data.frame"),
  prototype=list(data=data.frame()),
  contains="valuationTable"
);


#' A cohort life table obtained by joining two cohort life tables, each of which
#' applies only to certain observation years (e.g. for the past use the observed
#' PODs, and project them to the future with the trend projection)
#'
#' @slot table1 The first \code{valuationTable}, valid for years given in \code{yearRange1}
#' @slot yearRange1 The years, for which \code{table1} describes the death probabilities
#' @slot table2 The second \code{valuationTable}, valid for years given in \code{yearRange2}
#' @slot yearRange2 The years, for which \code{table2} describes the death probabilities
#'
#' @export valuationTable_joined
#' @exportClass valuationTable_joined
valuationTable_joined=setClass(
  "valuationTable_joined",
  slots=list(
    table1="valuationTable", yearRange1="numeric",
    table2="valuationTable", yearRange2="numeric"),
  contains="valuationTable"
);

#' A cohort life table obtained by mixing two life tables with the given weights
#'
#' @slot table1 The first \code{valuationTable}
#' @slot table2 The second \code{valuationTable}
#' @slot weight1 The weight of the first valuation table
#' @slot weight2 The weight of the second valuation table
#' @slot loading Additional security loading
#'
#' @export valuationTable_mixed
#' @exportClass valuationTable_mixed
valuationTable_mixed=setClass(
  "valuationTable_mixed",
  slots=c(table1="valuationTable", table2="valuationTable", weight1="numeric", weight2="numeric", loading="numeric"),
  prototype=list(weight1=1/2, weight2=1/2, loading=0),
  contains="valuationTable"
);



#' Return the maximum age of the life table
#'
#' @name valuationTable
#' @rdname valuationTable-class
#' @exportMethod getOmega
setGeneric("getOmega", function(object) standardGeneric("getOmega"));
#' Return the maximum age of the period life table
#'
#' @name valuationTable_period-class
#' @rdname valuationTable_period-class
#' @aliases getOmega,valuationTable_period-method
setMethod("getOmega", "valuationTable_period",
          function (object) {
            max(object@ages,na.rm=TRUE);
          })
#' Return the maximum age of the mixed life table
#'
#' @name valuationTable_mixed-class
#' @rdname valuationTable_mixed-class
#' @aliases getOmega,valuationTable_mixed-method
setMethod("getOmega", "valuationTable_mixed",
          function (object) {
            getOmega(object@table1);
          })
#' Return the maximum age of the joined life table
#'
#' @name valuationTable_joined-class
#' @rdname valuationTable_joined-class
#' @aliases getOmega,valuationTable_joined-method
setMethod("getOmega", "valuationTable_joined",
          function (object) {
            getOmega(object@table1);
          })


#' Return the defined ages of the life table
#'
#' @name valuationTable
#' @rdname valuationTable-class
#' @exportMethod ages
setGeneric("ages", function(object, ...) standardGeneric("ages"));
#' Return the defined ages of the life table
#'
#' @name valuationTable_period-class
#' @rdname valuationTable_period-class
#' @aliases ages,valuationTable_period-method
setMethod("ages", "valuationTable_period",
          function (object, ...) {
            object@ages;
          })
#' Return the defined ages of the life table
#'
#' @name valuationTable_mixed-class
#' @rdname valuationTable_mixed-class
#' @aliases ages,valuationTable_mixed-method
setMethod("ages", "valuationTable_mixed",
          function (object, ...) {
            ages(object@table1);
          })
#' Return the defined ages of the life table
#'
#' @name valuationTable_joined-class
#' @rdname valuationTable_joined-class
#' @aliases ages,valuationTable_joined-method
setMethod("ages", "valuationTable_joined",
          function (object, ...) {
            ages(object@table1);
          })



#' Return the age shift of the age-shifted life table given the birth year
#'
#' @param YOB The birth year for which the age shift should be determined.
#'
#' @name valuationTable_ageShift
#' @rdname valuationTable_ageShift-class
#' @exportMethod ageShift
setGeneric("ageShift", function(object, YOB=1975, ...) standardGeneric("ageShift"));
#' Return the age shift of the age-shifted life table given the birth year
#'
#' @param YOB The birth year for which the age shift should be determined.
#'
#' @name valuationTable_ageShift-class
#' @rdname valuationTable_ageShift-class
#' @aliases ageShift,valuationTable_ageShift-method
setMethod("ageShift","valuationTable_ageShift",
          function(object, YOB, ...) {
            shift = object@ageShifts[toString(YOB),];
            if (is.na(shift)) {
              # The row names (YOB) are unfortunately strings, so we cannot easily query them.
              # TODO: Change the data.frame to use a real column for the YOB
              firstYOB = utils::head(rownames(object@ageShifts), n=1);
              lastYOB = utils::tail(rownames(object@ageShifts), n=1);
              if (YOB < as.integer(firstYOB)) {
                shift = object@ageShifts[firstYOB,];
              } else if (YOB > as.integer(lastYOB)) {
                shift = object@ageShifts[lastYOB,];
              }
            }
            shift
          })



#' Return the (cohort) death probabilities of the life table given the birth year (if needed)
#'
#' @param YOB The birth year for which the death probabilities should be calculated
#'
#' @name valuationTable
#' @rdname valuationTable-class
#' @exportMethod deathProbabilities
setGeneric("deathProbabilities", function(object, ..., YOB=1975) standardGeneric("deathProbabilities"));
#' Return the (cohort) death probabilities of the life table given the birth year (if needed)
#'
#' @param YOB The birth year for which the death probabilities should be calculated
#'
#' @name valuationTable_period-class
#' @rdname valuationTable_period-class
#' @aliases deathProbabilities,valuationTable_period-method
setMethod("deathProbabilities", "valuationTable_period",
          function(object, ..., YOB=1975) {
            object@modification(object@deathProbs * (1+object@loading));
          })
#' Return the (cohort) death probabilities of the life table given the birth year (if needed)
#'
#' @param YOB The birth year for which the death probabilities should be calculated
#'
#' @name valuationTable_ageShift-class
#' @rdname valuationTable_ageShift-class
#' @aliases deathProbabilities,valuationTable_ageShift-method
setMethod("deathProbabilities","valuationTable_ageShift",
          function (object,  ..., YOB=1975) {
            qx=object@deathProbs * (1+object@loading);
            shift = ageShift(object, YOB);
            if (shift>0) {
              qx = c(qx[(shift+1):length(qx)], rep(qx[length(qx)], shift));
            } else if (shift<0) {
              qx = c(rep(0, -shift), qx[1:(length(qx)-(-shift))])
            }
            object@modification(qx)
          })

#' Return the (cohort) death probabilities of the life table given the birth year (if needed)
#'
#' @param YOB The birth year for which the death probabilities should be calculated
#'
#' @name valuationTable_trendProjection-class
#' @rdname valuationTable_trendProjection-class
#' @aliases deathProbabilities,valuationTable_trendProjection-method
setMethod("deathProbabilities","valuationTable_trendProjection",
          function (object,  ..., YOB=1975) {
            qx=object@deathProbs * (1+object@loading);
            if (is.null(object@trend2) || length(object@trend2)<=1) {
              ages=0:(length(qx)-1);
              damping=sapply(ages, function (age) { object@dampingFunction(YOB+age-object@baseYear) });
              # print(data.frame(age=0:(length(qx)-1), trend=object@trend, exponent=-object@trend*damping, damping=damping, baseqx=qx, qx=exp(-object@trend*damping)*qx)[66:90,]);
              finalqx=exp(-object@trend*damping)*qx;
            } else {
              # dampingFunction interpolates between the two trends:
              weights=sapply(YOB+0:(length(qx)-1), object@dampingFunction);
              finalqx=qx*exp(-(object@trend*(1-weights) + object@trend2*(weights))*(YOB+0:(length(qx)-1)-object@baseYear))
            }
            object@modification(finalqx)
          })

#' Return the (cohort) death probabilities of the life table given the birth year (if needed)
#'
#' @param YOB The birth year for which the death probabilities should be calculated
#'
#' @name valuationTable_improvementFactors-class
#' @rdname valuationTable_improvementFactors-class
#' @aliases deathProbabilities,valuationTable_improvementFactors-method
setMethod("deathProbabilities","valuationTable_improvementFactors",
          function (object,  ..., YOB=1975) {
            qx=object@deathProbs * (1+object@loading);
            finalqx=(1-object@improvement)^(YOB+0:(length(qx)-1)-object@baseYear)*qx;
            object@modification(finalqx)
          })
#' Return the (cohort) death probabilities of the life table given the birth year (if needed)
#'
#' @param YOB The birth year for which the death probabilities should be calculated
#'
#' @name valuationTable_mixed-class
#' @rdname valuationTable_mixed-class
#' @aliases deathProbabilities,valuationTable_mixed-method
setMethod("deathProbabilities","valuationTable_mixed",
          function (object,  ..., YOB=1975) {
            qx1=deathProbabilities(object@table1, ..., YOB) * (1+object@loading);
            qx2=deathProbabilities(object@table2, ..., YOB) * (1+object@loading);
            mixedqx=(object@weight1*qx1 + object@weight2*qx2)/(object@weight1 + object@weight2);
            object@modification(mixedqx)
          })




#' Return the (period) death probabilities of the life table for a given observation year
#'
#' @param Period  The observation year for which the period death probabilities should be determined
#'
#' @name valuationTable
#' @rdname valuationTable-class
#' @exportMethod periodDeathProbabilities
setGeneric("periodDeathProbabilities", function(object, ...) standardGeneric("periodDeathProbabilities"));
#' Return the (period) death probabilities of the life table for a given observation year
#'
#' @param Period  The observation year for which the period death probabilities should be determined
#'
#' @name valuationTable_period-class
#' @rdname valuationTable_period-class
#' @aliases periodDeathProbabilities,valuationTable_period-method
setMethod("periodDeathProbabilities", "valuationTable_period",
          function(object, ...) {
            object@modification(object@deathProbs * (1+object@loading));
          })
#' Return the (period) death probabilities of the life table for a given observation year
#'
#' @param Period  The observation year for which the period death probabilities should be determined
#'
#' @name valuationTable_ageShift-class
#' @rdname valuationTable_ageShift-class
#' @aliases periodDeathProbabilities,valuationTable_ageShift-method
setMethod("periodDeathProbabilities","valuationTable_ageShift",
          function (object,  ..., Period=1975) {
            # TODO
            qx=object@deathProbs * (1+object@loading);
            # TODO!!!
            # shift.index=match(YOB, object@shifts, 0);
            # if (shift.index) {}
            object@modification(qx)
          })
#' Return the (period) death probabilities of the life table for a given observation year
#'
#' @param Period  The observation year for which the period death probabilities should be determined
#'
#' @name valuationTable_trendProjection-class
#' @rdname valuationTable_trendProjection-class
#' @aliases periodDeathProbabilities,valuationTable_trendProjection-method
setMethod("periodDeathProbabilities","valuationTable_trendProjection",
          function (object,  ..., Period=1975) {
            qx=object@deathProbs * (1+object@loading);
            if (is.null(object@trend2) || length(object@trend2)<=1) {
              # ages=0:(length(qx)-1);
              damping=object@dampingFunction(Period-object@baseYear);
              # print(data.frame(age=0:(length(qx)-1), trend=object@trend, exponent=-object@trend*damping, damping=damping, baseqx=qx, qx=exp(-object@trend*damping)*qx)[66:90,]);
              finalqx=exp(-object@trend*damping)*qx;
            } else {
              # TODO!!!
              # dampingFunction interpolates between the two trends:
              # weights=sapply(YOB+0:(length(qx)-1), object@dampingFunction);
              # finalqx=qx*exp(-(object@trend*(1-weights) + object@trend2*(weights))*(YOB+0:(length(qx)-1)-object@baseYear));
            }
            object@modification(finalqx)
          })
#' Return the (period) death probabilities of the life table for a given observation year
#'
#' @param Period  The observation year for which the period death probabilities should be determined
#'
#' @name valuationTable_improvementFactors-class
#' @rdname valuationTable_improvementFactors-class
#' @aliases periodDeathProbabilities,valuationTable_improvementFactors-method
setMethod("periodDeathProbabilities","valuationTable_improvementFactors",
          function (object, ..., Period=1975) {
            qx=object@deathProbs * (1+object@loading);
            finalqx=(1-object@improvement)^(Period-object@baseYear)*qx;
            object@modification(finalqx)
          })
#' Return the (period) death probabilities of the life table for a given observation year
#'
#' @param Period  The observation year for which the period death probabilities should be determined
#'
#' @name valuationTable_mixed-class
#' @rdname valuationTable_mixed-class
#' @aliases periodDeathProbabilities,valuationTable_mixed-method
setMethod("periodDeathProbabilities","valuationTable_mixed",
          function (object,  ..., Period=1975) {
            qx1=periodDeathProbabilities(object@table1, ..., Period=Period) * (1+object@loading);
            qx2=periodDeathProbabilities(object@table2, ..., Period=Period) * (1+object@loading);
            mixedqx=(object@weight1*qx1 + object@weight2*qx2)/(object@weight1 + object@weight2);
            object@modification(mixedqx)
          })




#' Return the lifetable object (package lifecontingencies) for the cohort life table
#'
#' @param ... Parameters to be handed to the \code{deathProbabilities} method of the life table
#'
#' @name valuationTable
#' @rdname valuationTable-class
#' @exportMethod lifeTable
setGeneric("lifeTable", function(object, ...) standardGeneric("lifeTable"));
#' Return the lifetable object (package lifecontingencies) for the cohort life table
#'
#' @param ... Parameters to be handed to the \code{deathProbabilities} method of the life table
#'
#' @name valuationTable-class
#' @rdname valuationTable-class
#' @aliases lifeTable,valuationTable-method
setMethod("lifeTable","valuationTable",
          function (object,  ...) {
            qx=deathProbabilities(object, ...);
            if (qx[[length(qx)]]!=1) { qx=c(qx, 1, 1); }
            lifecontingencies::probs2lifetable(qx, type="qx")
          })




#' Return the base year of the life table
#'
#' @name valuationTable
#' @rdname valuationTable-class
#' @exportMethod baseYear
setGeneric("baseYear", function(object, ...) standardGeneric("baseYear"));
#' Return the base year of the life table
#'
#' @name valuationTable-class
#' @rdname valuationTable-class
#' @aliases baseYear,valuationTable-method
setMethod("baseYear","valuationTable",
          function (object,  ...) {
            object@baseYear
          })
#' Return the base year of the life table
#'
#' @name valuationTable_mixed-class
#' @rdname valuationTable_mixed-class
#' @aliases baseYear,valuationTable_mixed-method
setMethod("baseYear","valuationTable_mixed",
          function (object,  ...) {
            baseYear(object@table1)
          })



#' Return the base table of the life table
#'
#' @name valuationTable
#' @rdname valuationTable-class
#' @exportMethod baseTable
setGeneric("baseTable", function(object, ...) standardGeneric("baseTable"));
#' Return the base table of the life table
#'
#' @name valuationTable-class
#' @rdname valuationTable-class
#' @aliases baseTable,valuationTable-method
setMethod("baseTable","valuationTable",
          function (object,  ...) {
            c()
          })
#' Return the base table of the life table
#'
#' @name valuationTable_period-class
#' @rdname valuationTable_period-class
#' @aliases baseTable,valuationTable_period-method
setMethod("baseTable","valuationTable_period",
          function (object,  ...) {
            object@deathProbs
          })




#' Return the period life table as a \code{valuationTable_period} object
#'
#' @param Period The observation year, for which the death probabilities should be determined
#'
#' @name valuationTable
#' @rdname valuationTable-class
#' @exportMethod getPeriodTable
setGeneric("getPeriodTable", function(object, Period, ...) standardGeneric("getPeriodTable"));
#' Return the period life table as a \code{valuationTable_period} object
#'
#' @param Period The observation year, for which the death probabilities should be determined
#'
#' @name valuationTable-class
#' @rdname valuationTable-class
#' @aliases getPeriodTable,valuationTable-method
setMethod("getPeriodTable","valuationTable",
          function (object, Period, ...) {
            valuationTable_period(
                name = paste(object@name, ", Period ", Period),
                baseYear = Period,
                ages = ages(object),
                deathProbs = periodDeathProbabilities(object, Period=Period)
            )
          })



#' Return the cohort life table as a \code{valuationTable_period} object
#'
#' @param YOB The birth year for which the life table should be calculated
#'
#' @name valuationTable
#' @rdname valuationTable-class
#' @exportMethod getCohortTable
setGeneric("getCohortTable", function(object, YOB, ...) standardGeneric("getCohortTable"));
#' Return the cohort life table as a \code{valuationTable_period} object
#'
#' @param YOB The birth year for which the life table should be calculated
#'
#' @name valuationTable-class
#' @rdname valuationTable-class
#' @aliases getCohortTable,valuationTable-method
#' @export getCohortTable
setMethod("getCohortTable","valuationTable",
          function (object, YOB, ...) {
            valuationTable_period(
                name = paste(object@name, ", YOB ", YOB),
                baseYear = YOB,
                ages=ages(object),
                deathProbs=deathProbabilities(object, YOB=YOB)
            );
          })




#' Return a \code{valuationTable_trensProjection} object with the trend damping removed.
#'
#' @name valuationTable_trendProjection
#' @rdname valuationTable_trendProjection-class
#' @exportMethod undampenTrend
setGeneric("undampenTrend", function (object) standardGeneric("undampenTrend"));
#' Return a \code{valuationTable_trensProjection} object with the trend damping removed.
#'
#' @name valuationTable_trendProjection-class
#' @rdname valuationTable_trendProjection-class
#' @aliases undampenTrend,valuationTable_trendProjection-method
#' @export undampenTrend
setMethod("undampenTrend", "valuationTable_trendProjection",
          function (object) {
            object@dampingFunction=identity;
            object
            });
