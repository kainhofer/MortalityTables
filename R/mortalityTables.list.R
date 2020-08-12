#' List all available sets of life tables provided by the \link[MortalityTables]{MortalityTables-package} package
#' An existing life table can then be loaded with \link{mortalityTables.load}.
#'
#' @param pattern Restrict the results only to life table sets that match the pattern with wildcards (default: "*" to show all sets)
#' @param package The package that contains the desired dataset in its \code{extdata/}
#'                directory. Defaults to the "MortalityTables" package.
#'                Multiple packages can be given as a vector, even using regular expressions.
#' @param prefix The file prefix, defaults to MortalityTables. Can be overridden to list other types of files, like "PensionTables"
#'
#' @examples
#' mortalityTables.list()
#' mortalityTables.list("Austria_*")
#' mortalityTables.list("*Annuities")
#' mortalityTables.list(package = c("MyCustomPackage"))
#'
#' @export
mortalityTables.list = function(pattern = "*", package = c("^MortalityTables", "^PensionTables"), prefix = "MortalityTables") {
    ret = c()
    pkgs = utils::installed.packages()
    for (p in pkgs[,1]) {
        if (any(sapply(package, grepl, p))) { # package matches the pattern given as argument
            filepath = system.file("extdata", package = p);
            files = Sys.glob(file.path(filepath, paste(prefix, "_", pattern, ".R", sep = "")))
            ret = c(ret, gsub(paste('^', prefix, '_(.*).R$', sep = ""), '\\1', basename(files)))
        }
    }
    ret
}

#' List all available sets of pension tables provided by the \link[MortalityTables]{MortalityTables-package} package
#' An existing pension table can then be loaded with \link{pensionTables.load}.
#'
#' @param pattern Restrict the results only to pension table sets that match the pattern with wildcards (default: "*" to show all sets)
#' @param package The package that contains the desired dataset in its \code{extdata/}
#'                directory. Defaults to the "MortalityTables" package.
#'                Multiple packages can be given as a vector, even using regular expressions.
#'
#' @examples
#' pensionTables.list()
#' pensionTables.list("USA_*")
#' pensionTables.list(package = c("MyCustomPackage"))
#'
#' @export
pensionTables.list = function(pattern = "*", package = c("^MortalityTables", "^PensionTables")) {
    mortalityTables.list(pattern = pattern, package = package, prefix = "PensionTables")
}
