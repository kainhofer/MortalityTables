#' Load a named set of mortality tables provided by the \link{MortalityTables} package
#'
#' @param dataset The set of life tables to be loaded. A list of all available
#'                data sets is provided by the function \code{\link{mortalityTables.list}}.
#' @param wildcard Whether the dataset name contains wildcard. If TRUE, all
#'                 datasets matching the pattern will be loaded
#' @param package The package that contains the dataset in its \code{extdata/}
#'                directory. Defaults to the "MortalityTables" package.
#' @param prefix The prefix for the data sets (default is "MortalityTables")
#'
#' @export
mortalityTables.load = function(dataset, wildcard = FALSE, package = "MortalityTables", prefix = "MortalityTables") {
    if (wildcard) {
        sets = mortalityTables.list(dataset, package = package, prefix = prefix);
    } else {
        sets = c(dataset);
    }
    for (set in sets) {
        sname = gsub("[^-A-Za-z0-9_.]", "", set);
        message("Loading table dataset '", sname, "'");
        filename = system.file("extdata", paste(prefix, "_", sname, ".R", sep = ""), package = package);
        if (filename != "") {
            sys.source(filename, envir = globalenv())
        } else {
            warning(sprintf("Unable to locate dataset '%s' provided by the %s package!", sname, package));
        }
    }
}


#' Load a named set of pension tables provided by the \link{MortalityTables} package
#'
#' @param dataset The set of lifpensione tables to be loaded. A list of all available
#'                data sets is provided by the function \code{\link{pensionTables.list}}.
#' @param wildcard Whether the dataset name contains wildcard. If TRUE, all
#'                 datasets matching the pattern will be loaded
#' @param package The package that contains the dataset in its \code{extdata/}
#'                directory. Defaults to the "MortalityTables" package.
#'
#' @export
pensionTables.load = function(dataset, wildcard = FALSE, package = "MortalityTables") {
    mortalityTables.load(dataset = dataset, wildcard = wildcard, package = package, prefix = "PensionTables")
}


