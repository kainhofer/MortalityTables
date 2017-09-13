#' @import MortalityTables
NULL

stopifnot(require(methods), require(utils), require(MortalityTables)) # MortalityTable classes; new; Excel reader

my.natrim <- function(v, ...) {
    # Add a NA at the beginning and end, so the first and last entry of the rle
    # call will always be for a NA => Use them to call head and tail accordingly:
    vv = c(NA, v, NA);
    r = rle(!is.na(vv));
    tail(head(vv, -tail(r$lengths, 1)), -head(r$lengths, 1))
}

###############################################################################
# USA: RP2014 pension plan table with MP2014 mortality improvement factors
###############################################################################

RP2014.data = utils::read.csv(
    system.file("extdata", "USA_PensionPlans_RP2014.csv", package = "MortalityTables"),
    skip = 4,
    header = FALSE,
    col.names = c(
        "age",
        "qax", "qpx", "qix",
        "qay", "qpy", "qiy",
        "SPACER",
        "qax_blue", "qpx_blue",
        "qay_blue", "qpy_blue",
        "SPACER",
        "qax_white", "qpx_white",
        "qay_white", "qpy_white",
        "SPACER",
        "age_young", "qx_young", "qy_young")
    #, colClasses = c(age = "numeric", qax = "numeric", qpx = "numeric", qix = "numeric", qay = "numeric", qpy = "numeric", qiy = "numeric")
);

RP2014.readImprovements = function(file) {
    data = as.matrix(utils::read.csv(
        system.file("extdata", "USA_PensionPlans_MP2014_Male.csv", package = "MortalityTables"),
        skip = 1,
        check.names = FALSE,
        header = TRUE,
        row.names = 1
    ))
    cn = colnames(data)
    lastyear = tail(cn,1)
    lastyear = substr(lastyear, 1, nchar(lastyear) - 1);
    colnames(data) = c(head(cn, -1), lastyear)
    bel20 = data["≤ 20",];
    young = matrix(bel20, ncol = length(bel20), nrow = 21, byrow = TRUE, dimnames = list(0:20, colnames(data)))
    rbind(young, data[-1,])
}

RP2014.improvement.male = RP2014.readImprovements("USA_PensionPlans_MP2014_Male.csv");
RP2014.improvement.female = RP2014.readImprovements("USA_PensionPlans_MP2014_Female.csv");


nameRP14 = function(name = "", desc = "") {
    paste("RP2014", name, ", ", desc, sep = "")
}

tableRP14 = function(name, data = data, agevar = "age", probvar, improvement = NULL, ..., baseyear = 2014) {
    if (is.null(improvement)) {
        mortalityTable.period(
            name = name, ages = data[[agevar]], baseYear = baseyear,
            deathProbs = data[[probvar]], ...)
    } else {
        mortalityTable.improvementFactors(
            name = name, ages = data[[agevar]], baseYear = baseyear,
            deathProbs = data[[probvar]], improvement = improvement, ...)
    }
}

RP2014.zeroes = mortalityTable.zeroes(name = "No transition", ages = RP2014.data[["age"]])


################################################################################
# Total Dataset (RP-2014 with MP-2014 improvements)
################################################################################
name = "";

RP2014.male = pensionTable(
    name = nameRP14(desc = "male"),
    baseYear = 2014,
    qx =  tableRP14(nameRP14(name, "qax, active males"), RP2014.data, "age", "qax", improvement = RP2014.improvement.male),
    # The RP2014 table does not contain any invalidity probabilities
    ix = RP2014.zeroes,
    qgx = RP2014.zeroes,
    qix = tableRP14(nameRP14(name, "qix, disabled males"), RP2014.data, "age", "qix", improvement = RP2014.improvement.male),
    rx =  RP2014.zeroes,
    apx = RP2014.zeroes,
    apix = RP2014.zeroes,
    qpx = tableRP14(nameRP14(name, "qpx, retired males"), RP2014.data, "age", "qpx", improvement = RP2014.improvement.male),
    hx =  RP2014.zeroes,
    qwy = RP2014.zeroes,
    yx =  RP2014.zeroes
)
RP2014.female = pensionTable(
    name = nameRP14(desc = "female"),
    baseYear = 2014,
    qx =  tableRP14(nameRP14(name, "qay, active females"), RP2014.data, "age", "qay", improvement = RP2014.improvement.female),
    # The RP2014 table does not contain any invalidity probabilities
    ix = RP2014.zeroes,
    qgx = RP2014.zeroes,
    qix = tableRP14(nameRP14(name, "qiy, disabled females"), RP2014.data, "age", "qiy", improvement = RP2014.improvement.female),
    rx =  RP2014.zeroes,
    apx = RP2014.zeroes,
    apix = RP2014.zeroes,
    qpx = tableRP14(nameRP14(name, "qpy, retired females"), RP2014.data, "age", "qpy", improvement = RP2014.improvement.female),
    hx =  RP2014.zeroes,
    qwy = RP2014.zeroes,
    yx =  RP2014.zeroes
)


################################################################################
# White collar dataset (RP-2014 with MP-2014 improvements)
################################################################################
name = "White Collar";

RP2014.male.whitecollar = pensionTable(
    name = nameRP14(desc = "male"),
    baseYear = 2014,
    qx =  tableRP14(nameRP14(name, "qax, active males"), RP2014.data, "age", "qax_white", improvement = RP2014.improvement.male),
    # The RP2014 table does not contain any invalidity probabilities
    ix = RP2014.zeroes,
    qgx = RP2014.zeroes,
    qix = tableRP14(nameRP14(name, "qix, disabled males"), RP2014.data, "age", "qix", improvement = RP2014.improvement.male),
    rx =  RP2014.zeroes,
    apx = RP2014.zeroes,
    apix = RP2014.zeroes,
    qpx = tableRP14(nameRP14(name, "qpx, retired males"), RP2014.data, "age", "qpx_white", improvement = RP2014.improvement.male),
    hx =  RP2014.zeroes,
    qwy = RP2014.zeroes,
    yx =  RP2014.zeroes
)
RP2014.female.whitecollar = pensionTable(
    name = nameRP14(desc = "female"),
    baseYear = 2014,
    qx =  tableRP14(nameRP14(name, "qay, active females"), RP2014.data, "age", "qay_white", improvement = RP2014.improvement.female),
    # The RP2014 table does not contain any invalidity probabilities
    ix = RP2014.zeroes,
    qgx = RP2014.zeroes,
    qix = tableRP14(nameRP14(name, "qiy, disabled females"), RP2014.data, "age", "qiy", improvement = RP2014.improvement.female),
    rx =  RP2014.zeroes,
    apx = RP2014.zeroes,
    apix = RP2014.zeroes,
    qpx = tableRP14(nameRP14(name, "qpy, retired females"), RP2014.data, "age", "qpy_white", improvement = RP2014.improvement.female),
    hx =  RP2014.zeroes,
    qwy = RP2014.zeroes,
    yx =  RP2014.zeroes
)


################################################################################
# Blue collar dataset (RP-2014 with MP-2014 improvements)
################################################################################
name = "Blue Collar";

RP2014.male.bluecollar = pensionTable(
    name = nameRP14(desc = "male"),
    baseYear = 2014,
    qx =  tableRP14(nameRP14(name, "qax, active males"), RP2014.data, "age", "qax_blue", improvement = RP2014.improvement.male),
    # The RP2014 table does not contain any invalidity probabilities
    ix = RP2014.zeroes,
    qgx = RP2014.zeroes,
    qix = tableRP14(nameRP14(name, "qix, disabled males"), RP2014.data, "age", "qix", improvement = RP2014.improvement.male),
    rx =  RP2014.zeroes,
    apx = RP2014.zeroes,
    apix = RP2014.zeroes,
    qpx = tableRP14(nameRP14(name, "qpx, retired males"), RP2014.data, "age", "qpx_blue", improvement = RP2014.improvement.male),
    hx =  RP2014.zeroes,
    qwy = RP2014.zeroes,
    yx =  RP2014.zeroes
)
RP2014.female.bluecollar = pensionTable(
    name = nameRP14(desc = "female"),
    baseYear = 2014,
    qx =  tableRP14(nameRP14(name, "qay, active females"), RP2014.data, "age", "qay_blue", improvement = RP2014.improvement.female),
    # The RP2014 table does not contain any invalidity probabilities
    ix = RP2014.zeroes,
    qgx = RP2014.zeroes,
    qix = tableRP14(nameRP14(name, "qiy, disabled females"), RP2014.data, "age", "qix", improvement = RP2014.improvement.female),
    rx =  RP2014.zeroes,
    apx = RP2014.zeroes,
    apix = RP2014.zeroes,
    qpx = tableRP14(nameRP14(name, "qpy, retired females"), RP2014.data, "age", "qpy_blue", improvement = RP2014.improvement.female),
    hx =  RP2014.zeroes,
    qwy = RP2014.zeroes,
    yx =  RP2014.zeroes
)


rm(RP2014.data, tableRP14, nameRP14, RP2014.readImprovements, RP2014.zeroes)

