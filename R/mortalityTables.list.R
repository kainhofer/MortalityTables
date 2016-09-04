#' List all available sets of life tables provided by the \link[MortalityTables]{MortalityTables-package} package
#' An existing life table can then be loaded with \link{mortalityTables.load}.
#'
#' @param pattern Restrict the results only to life table sets that match the pattern (default: "*" to show all sets)
#'
#' @export
mortalityTables.list = function(pattern="*") {
    filepath = system.file("extdata", package="MortalityTables");
    files = Sys.glob(file.path(filepath, paste("MortalityTables_", pattern, ".R", sep="")))
    gsub('^MortalityTables_(.*).R$', '\\1', basename(files))
}


