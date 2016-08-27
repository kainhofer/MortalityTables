#' Load a named set of life tables provided by the \link{ValuationTables} package
#'
#' @param dataset The set of life tables to be loaded. A list of all available data sets is provided by the
#'
#' @export
valuationTables.load = function(dataset, wildcard=FALSE) {
    if (wildcard) {
        sets = valuationTables.list(dataset);
    } else {
        sets = c(dataset);
    }
    for (set in sets) {
        sname = gsub("[^-A-Za-z0-9_.]", "", set);
        message("Loading valuation life table data set '", sname, "'");
        filename = system.file("extdata", paste("ValuationTables_", sname, ".R", sep = ""), package="ValuationTables");
        if (filename != "") {
            sys.source(filename, envir = globalenv())
            #envir=topenv())
        } else {
            warning(sprintf("Unable to locate dataset '%s' provided by the ValuationTables package!", sname));
        }
    }
}
