#' List all available sets of life tables provided by the \link[MortalityTables]{MortalityTables-package} package
#' An existing life table can then be loaded with \link{mortalityTables.load}.
#'
#' @param pattern Restrict the results only to life table sets that match the pattern (default: "*" to show all sets)
#' @param package The package that contains the desired dataset in its \code{extdata/}
#'                directory. Defaults to the "MortalityTables" package.
#'
#' @export
mortalityTables.list = function(pattern = "*", package = "MortalityTables", prefix = "MortalityTables") {
    filepath = system.file("extdata", package = package);
    files = Sys.glob(file.path(filepath, paste(prefix, "_", pattern, ".R", sep = "")))
    gsub(paste('^', prefix, '_(.*).R$', sep = ""), '\\1', basename(files))
}


#' List all available sets of pension tables provided by the \link[MortalityTables]{MortalityTables-package} package
#' An existing pension table can then be loaded with \link{pensionTables.load}.
#'
#' @param pattern Restrict the results only to pension table sets that match the pattern (default: "*" to show all sets)
#' @param package The package that contains the desired dataset in its \code{extdata/}
#'                directory. Defaults to the "MortalityTables" package.
#'
#' @export
pensionTables.list = function(pattern = "*", package = "MortalityTables") {
    mortalityTables.list(pattern = pattern, package = package, prefix = "PensionTables")
}
