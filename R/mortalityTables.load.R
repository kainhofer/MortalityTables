#' Load a named set of mortality tables provided by the \link{MortalityTables} package
#'
#' @param dataset The set(s) of life tables to be loaded. A list of all available
#'                data sets is provided by the function \code{\link{mortalityTables.list}}.
#'                Wildcards (*) are allowed to match and load multiple datasets.
#' @param package The package that contains the dataset in its \code{extdata/}
#'                directory. Defaults to all packages starting with names that
#'                start with "MortalityTables" or "PensionTables".
#'                Multiple packages can be given as a vector, even using regular expressions.
#' @param prefix The prefix for the data sets (default is "MortalityTables").
#'
#' @examples
#' mortalityTables.list()
#' mortalityTables.load("Austria_Annuities_*")
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#' mortalityTables.load("*Annuities")
#' mortalityTables.load("MyCustomTable", package = c("MyCustomPackage"))
#'
#' @export
mortalityTables.load = function(dataset, package = c("^MortalityTables", "^PensionTables"), prefix = "MortalityTables") {
    sets = mortalityTables.list(dataset, package = package, prefix = prefix);
    if (length(sets) == 0) {
        warning(sprintf("Unable to locate dataset '%s' provided by the %s package!", dataset, paste(c(package), collapse = " or ")));
    }
    pkgs = installed.packages()
    for (set in sets) {
        sname = gsub("[^-A-Za-z0-9_.]", "", set);
        message("Loading table dataset '", sname, "'");
        loaded = FALSE;
        for (p in pkgs[,1]) {
            if (any(sapply(package, grepl, p))) { # package matches the pattern given as argument
                filename = system.file("extdata", paste(prefix, "_", sname, ".R", sep = ""), package = p);
                if (filename != "") {
                    # Make sure the providing package is loaded, in case it provides helper functions
                    require(p, character.only = TRUE)

                    # Taken from the definition of sys.source and adjusted to include the
                    # encoding (required for Windows, otherwise UTF8-strings will be broken!)
                    lines = readLines(filename, encoding = "UTF-8", warn = FALSE)
                    srcfile = srcfilecopy(filename, lines, file.mtime(filename), isFile = TRUE)
                    exprs = parse(text = lines, srcfile = srcfile, keep.source = TRUE)
                    for (i in seq_along(exprs))
                        eval(exprs[i], envir = globalenv())

                    # sys.source(filename, envir = globalenv())
                    loaded = TRUE
                }
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
#'                directory. Defaults to all packages starting with names that
#'                start with "MortalityTables" or "PensionTables".
#'                Multiple packages can be given as a vector, even using regular expressions.
#'
#' pensionTables.list()
#' pensionTables.load("*")
#' pensionTables.load("USA_PensionPlan_RP2014")
#'
#' @export
pensionTables.load = function(dataset, package = c("^MortalityTables", "^PensionTables")) {
    mortalityTables.load(dataset = dataset, package = package, prefix = "PensionTables")
}

