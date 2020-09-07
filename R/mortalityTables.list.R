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
    # TODO: Generalize lib.loc to a function parameter
    res = c()

    for (p in pattern) {
        # We want all files that are of the following form:
        #   [LIBDIR]/MortalityTables*/extdata/[PREFIX]_[NAME].R and return the list of all [NAME] parts

        lib.loc <- .libPaths()
        # Get a list of all directories for MortalityTables / PensionTable extensions packages
        packs = unlist(lapply(package, FUN = function(p) { list.files(lib.loc, p, full.names = TRUE)}))
        # From those directories, list all extdata/[prefix]_[pattern].R files
        files = Sys.glob(file.path(packs, "extdata", paste(prefix, "_", pattern, ".R", sep = "")))
        # Extract the name, i.e. everything after the prefix and without the .R:
        res = c(res, gsub(paste('^', prefix, '_(.*).R$', sep = ""), '\\1', basename(files)))
    }
    res
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
