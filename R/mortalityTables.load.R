#' Load a named set of mortality tables provided by the \link{MortalityTables} package
#'
#' @param dataset The set(s) of life tables to be loaded. A list of all available
#'                data sets is provided by the function \code{\link{mortalityTables.list}}.
#'                Wildcards (*) are allowed to match and load multiple datasets.
#' @param package The package that contains the dataset in its \code{extdata/}
#'                directory. Defaults to the "MortalityTables" package.
#'                Multiple packages can be given as a vector.
#' @param prefix The prefix for the data sets (default is "MortalityTables").
#'
#' @export
mortalityTables.load = function(dataset, package = c("MortalityTables", "MortalityTablesPrivate"), prefix = "MortalityTables") {
    sets = mortalityTables.list(dataset, package = package, prefix = prefix);
    if (length(sets) == 0) {
        warning(sprintf("Unable to locate dataset '%s' provided by the %s package!", dataset, paste(c(package), collapse = " or ")));
    }
    for (set in sets) {
        sname = gsub("[^-A-Za-z0-9_.]", "", set);
        message("Loading table dataset '", sname, "'");
        loaded = FALSE;
        for (p in c(package)) {
            filename = system.file("extdata", paste(prefix, "_", sname, ".R", sep = ""), package = p);
            if (filename != "") {
                sys.source(filename, envir = globalenv())
                loaded = TRUE
            }
        }
        if (!loaded) {
            warning(sprintf("Unable to locate dataset '%s' provided by the %s package!", sname, package));
        }
    }
}


#' Load a named set of pension tables provided by the \link{MortalityTables} package
#'
#' @param dataset The set of lifpensione tables to be loaded. A list of all available
#'                data sets is provided by the function \code{\link{pensionTables.list}}.
#'                Wildcards (*) are allowed to match and load multiple datasets.
#' @param package The package that contains the dataset in its \code{extdata/}
#'                directory. Defaults to the "MortalityTables" package.
#'                Multiple packages can be given as a vector.
#'
#' @export
pensionTables.load = function(dataset, package = c("MortalityTables", "MortalityTablesPrivate")) {
    mortalityTables.load(dataset = dataset, package = package, prefix = "PensionTables")
}


