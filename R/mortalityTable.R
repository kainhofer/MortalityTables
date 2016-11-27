#' Provide life table classes for life insurance purposes
#'
#' @import methods
#' @import ggplot2
#'
"_PACKAGE"


#' Class mortalityTable
#'
#' Class \code{mortalityTable} is the (virtual) base class for all mortality
#' tables. It contains the name and some general values applying to all
#' types of tables, but does not contain any data itself. Use a child class
#' to create actual mortality tables.
#'
#' @slot name     The human-readable name of the mortality table
#' @slot baseYear The base year of the mortality table (e.g. for tables with trend projection)
#' @slot modification A function that will be called with the final death probabilities
#'        to give the user a way to modify the final probabilities
#' @slot loading  Additional security loading on the resulting table (single numeric
#'        value, e.g. 0.05 adds 5\% security margin to the probabilities)
#'
#' @export mortalityTable
#' @exportClass mortalityTable
mortalityTable = setClass(
    "mortalityTable",
    slots = list(
        name = "character",
        baseYear = "numeric",
        loading = "numeric",
        modification = "function"
    ),
    prototype = list(
        name = "Actuarial Mortality Table",
        baseYear = 0,
        loading = 0,
        modification = identity
    ),
    contains = "VIRTUAL"
)
