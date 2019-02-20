stopifnot(require(methods), require(utils), require(MortalityTables)) # MortalityTable classes; new; Excel reader


###############################################################################
### Volkszählungen Österreich
###############################################################################


a.vz.dataM = utils::read.csv(system.file("extdata", "Austria_Census_Male.csv", package = "MortalityTables"), skip = 3);
a.vz.dataF = utils::read.csv(system.file("extdata", "Austria_Census_Female.csv", package = "MortalityTables"), skip = 3);
a.vz.dataU = utils::read.csv(system.file("extdata", "Austria_Census_Unisex.csv", package = "MortalityTables"), skip = 3);

censtable = function(data, name, qslot, baseYear = 1900, sex = "m") {
  qx = data[names(data) == qslot];
  ix = complete.cases(qx);
  mortalityTable.period(name = name, ages = data$x[ix], deathProbs = qx[ix,], baseYear = baseYear,
      data = list(
          dim = list(sex = sex, collar = "Gesamtbevölkerung", type = "Volkssterbetafel Österreich", data = "official", year = baseYear)
      )
  )
}

mort.AT.census.1869.male = censtable(a.vz.dataM, name = "ÖVSt 1868/71 M",   baseYear = 1869, qslot = "X1868.71", sex = "m")
mort.AT.census.1880.male = censtable(a.vz.dataM, name = "ÖVSt 1879/82 M",   baseYear = 1880, qslot = "X1879.82", sex = "m")
mort.AT.census.1890.male = censtable(a.vz.dataM, name = "ÖVSt 1889/92 M",   baseYear = 1890, qslot = "X1889.92", sex = "m")
mort.AT.census.1900.male = censtable(a.vz.dataM, name = "ÖVSt 1899/1902 M", baseYear = 1900, qslot = "X1899.1902", sex = "m")
mort.AT.census.1910.male = censtable(a.vz.dataM, name = "ÖVSt 1909/12 M",   baseYear = 1910, qslot = "X1909.12", sex = "m")
mort.AT.census.1931.male = censtable(a.vz.dataM, name = "ÖVSt 1930/33 M",   baseYear = 1931, qslot = "X1930.33", sex = "m")
mort.AT.census.1951.male = censtable(a.vz.dataM, name = "ÖVSt 1949/51 M",   baseYear = 1951, qslot = "X1949.51", sex = "m")
mort.AT.census.1961.male = censtable(a.vz.dataM, name = "ÖVSt 1959/61 M",   baseYear = 1961, qslot = "X1959.61", sex = "m")
mort.AT.census.1971.male = censtable(a.vz.dataM, name = "ÖVSt 1970/72 M",   baseYear = 1971, qslot = "X1970.72", sex = "m")
mort.AT.census.1981.male = censtable(a.vz.dataM, name = "ÖVSt 1980/82 M",   baseYear = 1981, qslot = "X1980.82", sex = "m")
mort.AT.census.1991.male = censtable(a.vz.dataM, name = "ÖVSt 1990/92 M",   baseYear = 1991, qslot = "X1990.92", sex = "m")
mort.AT.census.2001.male = censtable(a.vz.dataM, name = "ÖVSt 2000/02 M",   baseYear = 2001, qslot = "X2000.02", sex = "m")
mort.AT.census.2011.male = censtable(a.vz.dataM, name = "ÖVSt 2010/2012 M", baseYear = 2011, qslot = "X2010.12", sex = "m")

mort.AT.census.1869.female = censtable(a.vz.dataF, name = "ÖVSt 1868/71 F",   baseYear = 1869, qslot = "X1868.71", sex = "w")
mort.AT.census.1880.female = censtable(a.vz.dataF, name = "ÖVSt 1879/82 F",   baseYear = 1880, qslot = "X1879.82", sex = "w")
mort.AT.census.1890.female = censtable(a.vz.dataF, name = "ÖVSt 1889/92 F",   baseYear = 1890, qslot = "X1889.92", sex = "w")
mort.AT.census.1900.female = censtable(a.vz.dataF, name = "ÖVSt 1899/1902 F", baseYear = 1900, qslot = "X1899.1902", sex = "w")
mort.AT.census.1910.female = censtable(a.vz.dataF, name = "ÖVSt 1909/12 F",   baseYear = 1910, qslot = "X1909.12", sex = "w")
mort.AT.census.1931.female = censtable(a.vz.dataF, name = "ÖVSt 1930/33 F",   baseYear = 1931, qslot = "X1930.33", sex = "w")
mort.AT.census.1951.female = censtable(a.vz.dataF, name = "ÖVSt 1949/51 F",   baseYear = 1951, qslot = "X1949.51", sex = "w")
mort.AT.census.1961.female = censtable(a.vz.dataF, name = "ÖVSt 1959/61 F",   baseYear = 1961, qslot = "X1959.61", sex = "w")
mort.AT.census.1971.female = censtable(a.vz.dataF, name = "ÖVSt 1970/72 F",   baseYear = 1971, qslot = "X1970.72", sex = "w")
mort.AT.census.1981.female = censtable(a.vz.dataF, name = "ÖVSt 1980/82 F",   baseYear = 1981, qslot = "X1980.82", sex = "w")
mort.AT.census.1991.female = censtable(a.vz.dataF, name = "ÖVSt 1990/92 F",   baseYear = 1991, qslot = "X1990.92", sex = "w")
mort.AT.census.2001.female = censtable(a.vz.dataF, name = "ÖVSt 2000/02 F",   baseYear = 2001, qslot = "X2000.02", sex = "w")
mort.AT.census.2011.female = censtable(a.vz.dataF, name = "ÖVSt 2010/2012 F", baseYear = 2011, qslot = "X2010.12", sex = "w")

mort.AT.census.2001.unisex = mortalityTable.mixed(table1 = mort.AT.census.2001.male, table2 = mort.AT.census.2001.female,
    data = list(
        dim = list(sex = "u", collar = "Gesamtbevölkerung", type = "Volkssterbetafel Österreich", data = "official", year = 2001)
    )
)
mort.AT.census.2011.unisex = censtable(a.vz.dataU, name = "ÖVSt 2010/2012 U", baseYear = 2011, qslot = "X2010.12", sex = "u")

mort.AT.census.ALL.male = MortalityTables::makeQxDataFrame(
              mort.AT.census.1869.male,
              mort.AT.census.1880.male,
              mort.AT.census.1890.male,
              mort.AT.census.1900.male,
              mort.AT.census.1910.male,
              mort.AT.census.1931.male,
              mort.AT.census.1951.male,
              mort.AT.census.1961.male,
              mort.AT.census.1971.male,
              mort.AT.census.1981.male,
              mort.AT.census.1991.male,
              mort.AT.census.2001.male,
              mort.AT.census.2011.male);

mort.AT.census.ALL.female = MortalityTables::makeQxDataFrame(
              mort.AT.census.1869.female,
              mort.AT.census.1880.female,
              mort.AT.census.1890.female,
              mort.AT.census.1900.female,
              mort.AT.census.1910.female,
              mort.AT.census.1931.female,
              mort.AT.census.1951.female,
              mort.AT.census.1961.female,
              mort.AT.census.1971.female,
              mort.AT.census.1981.female,
              mort.AT.census.1991.female,
              mort.AT.census.2001.female,
              mort.AT.census.2011.female);

rm(a.vz.dataM, a.vz.dataF, censtable)

###############################################################################

# plot(mort.AT.census.ALL.male, title = "Vergleich österreichische Sterbetafeln, Männer", legend.position = c(1,0))
# plot(mort.AT.census.ALL.female, title = "Vergleich österreichische Sterbetafeln, Frauen", legend.position = c(1,0))

