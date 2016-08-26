#' Provide life table classes for life insurance purposes
#'
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
#'        value, e.g. 0.05 adds 5\% security margin to the probabilities)
#'
#' @export valuationTable
#' @exportClass valuationTable
valuationTable=setClass(
    "valuationTable",
    slots = list(
        name = "character",
        baseYear = "numeric",
        loading = "numeric",
        modification = "function"
    ),
    prototype = list(
        name = "Actuarial Valuation Table",
        baseYear = 2000,
        loading = 0,
        modification = identity
    ),
    contains = "VIRTUAL"
)
