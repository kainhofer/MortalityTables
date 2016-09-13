#' Load a named set of mortality tables provided by the \link{MortalityTables} package
#'
#' @param dataset The set of life tables to be loaded. A list of all available
#'                data sets is provided by the function \code{\link{mortalityTables.list}}.
#' @param wildcard Whether the dataset name contains wildcard. If TRUE, all
#'                 datasets matching the pattern will be loaded
#' @param package The package that contains the dataset in its \code{extdata/}
#'                directory. Defaults to the "MortalityTables" package.
#'
#' @export
mortalityTables.load = function(dataset, wildcard=FALSE, package="MortalityTables") {
    if (wildcard) {
        sets = mortalityTables.list(dataset, package = package);
    } else {
        sets = c(dataset);
    }
    for (set in sets) {
        sname = gsub("[^-A-Za-z0-9_.]", "", set);
        message("Loading mortality table data set '", sname, "'");
        filename = system.file("extdata", paste("MortalityTables_", sname, ".R", sep = ""), package = package);
        if (filename != "") {
            sys.source(filename, envir = globalenv())
        } else {
            warning(sprintf("Unable to locate dataset '%s' provided by the %s package!", sname, package));
        }
    }
}
