
if(is.na(match("data:MortalityTables", search()))) {
    mortalityTables.environment = attach(what = NULL, name = "data:MortalityTables")
} else {
    mortalityTables.environment = as.environment("data:MortalityTables")
}
print(search())
# mortalityTables.environment = NULL

# mortalityTables.environment = mortalityTables.environment
# new.env()
# attr(mortalityTables.environment, "name") = "data:MortalityTables"

.onLoad = function(libname, pkgname) {
    print(search())
    str(mortalityTables.environment)
    #    assign("mortalityTables.environment1", mortalityTables.environment, envir = as.environment("data:MortalityTables"))
    assign("mortalityTables.environment.onload", mortalityTables.environment, envir = mortalityTables.environment)
    str(mortalityTables.environment)
    print(search())
}

.onAttach = function(libname, pkgname) {
    print(search())
    str(mortalityTables.environment)
    # attach(mortalityTables.environment, name = "data:MortalityTables")
    #    assign("mortalityTables.environment1", mortalityTables.environment, envir = as.environment("package:MortalityTables"))
    assign("mortalityTables.environment.onAttach",mortalityTables.environment, envir = mortalityTables.environment)
    str(mortalityTables.environment)
    print(search())
}

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
#' @examples
#' mortalityTables.list()
#' mortalityTables.load("Austria_Annuities_*")
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#' mortalityTables.load("*Annuities")
#' mortalityTables.load("MyCustomTable", package = c("MyCustomPackage"))
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
                # sys.source(filename, envir = mortalityTables.environment)
                sys.source(filename, envir = globalenv())
                loaded = TRUE
            }
        }
        if (!loaded) {
            warning(sprintf("Unable to locate dataset '%s' provided by the %s package!", sname, package));
        }
    }
    assign(x = "loaded", value = dataset, envir = mortalityTables.environment)
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
#' pensionTables.list()
#' pensionTables.load("*")
#' pensionTables.load("USA_PensionPlan_RP2014")
#'
#' @export
pensionTables.load = function(dataset, package = c("MortalityTables", "MortalityTablesPrivate")) {
    mortalityTables.load(dataset = dataset, package = package, prefix = "PensionTables")
}


