#' List all available sets of life tables provided by the \link[MortalityTables]{MortalityTables-package} package
#' An existing life table can then be loaded with \link{mortalityTables.load}.
#'
#' @param pattern Restrict the results only to life table sets that match the pattern (default: "*" to show all sets)
#' @param package The package that contains the desired dataset in its \code{extdata/}
#'                directory. Defaults to the "MortalityTables" package.
#'                Multiple packages can be given as a vector.
#' @param prefix The file prefix, defaults to MortalityTables. Can be overridden to list other types of files, like "PensionTables"
#'
#' @export
mortalityTables.list = function(pattern = "*", package = c("MortalityTables", "MortalityTablesPrivate"), prefix = "MortalityTables") {
    ret = c()
    for (p in c(package)) {
        filepath = system.file("extdata", package = p);
        files = Sys.glob(file.path(filepath, paste(prefix, "_", pattern, ".R", sep = "")))
        ret = c(ret, gsub(paste('^', prefix, '_(.*).R$', sep = ""), '\\1', basename(files)))
    }
    ret
}


#' List all available sets of pension tables provided by the \link[MortalityTables]{MortalityTables-package} package
#' An existing pension table can then be loaded with \link{pensionTables.load}.
#'
#' @param pattern Restrict the results only to pension table sets that match the pattern (default: "*" to show all sets)
#' @param package The package that contains the desired dataset in its \code{extdata/}
#'                directory. Defaults to the "MortalityTables" package.
#'                Multiple packages can be given as a vector.
#'
#' @export
pensionTables.list = function(pattern = "*", package = c("MortalityTables", "MortalityTablesPrivate")) {
    mortalityTables.list(pattern = pattern, package = package, prefix = "PensionTables")
}
