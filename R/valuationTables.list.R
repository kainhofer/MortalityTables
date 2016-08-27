#' List all available sets of life tables provided by the \link[ValuationTables]{ValuationTables-package} package
#' An existing life table can then be loaded with \link{valuationTables.load}.
#'
#' @param pattern Restrict the results only to life table sets that match the pattern (default: "*" to show all sets)
#'
#' @export
valuationTables.list = function(pattern="*") {
    filepath = system.file("extdata", package="ValuationTables");
    files = Sys.glob(file.path(filepath, paste("ValuationTables_", pattern, ".R", sep="")))
    gsub('^ValuationTables_(.*).R$', '\\1', basename(files))
}


