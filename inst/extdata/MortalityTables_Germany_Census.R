stopifnot(require(methods), require(utils), require(MortalityTables))


###############################################################################
### Volkszählungen Deutschland
###############################################################################


de.vz.dataDR = utils::read.csv(system.file("extdata", "Germany_Census_DeutschesReich.csv", package="MortalityTables"), skip=3);
de.vz.dataBRD = utils::read.csv(system.file("extdata", "Germany_Census_BRD.csv", package="MortalityTables"), skip=3);

censtable = function(data, name, qslot, baseYear=1900) {
    qx=data[names(data)==qslot];
    ix=complete.cases(qx);
    mortalityTable.period(name=name, ages=data$Alter[ix], deathProbs=qx[ix,], baseYear=baseYear)
}

mort.DE.census.1871.81.male = censtable(de.vz.dataDR, name="ADSt 1871/81 M",   baseYear=1869, qslot="ADSt.1871.81.M");
mort.DE.census.1881.90.male = censtable(de.vz.dataDR, name="ADSt 1881/90 M",   baseYear=1869, qslot="ADSt.1881.90.M");
mort.DE.census.1891.1900.male = censtable(de.vz.dataDR, name="ADSt 1891/1900 M",   baseYear=1869, qslot="ADSt.1891.1900.M");
mort.DE.census.1901.10.male = censtable(de.vz.dataDR, name="ADSt 1901/10 M",   baseYear=1869, qslot="ADSt.1901.10.M");
mort.DE.census.1910.11.male = censtable(de.vz.dataDR, name="ADSt 1910/11 M",   baseYear=1869, qslot="ADSt.1910.11.M");
mort.DE.census.1924.26.male = censtable(de.vz.dataDR, name="ADSt 1924/26 M",   baseYear=1869, qslot="ADSt.1924.26.M");
mort.DE.census.1932.34.male = censtable(de.vz.dataDR, name="ADSt 1932/34 M",   baseYear=1869, qslot="ADSt.1932.34.M");
mort.DE.census.1949.51.male = censtable(de.vz.dataBRD, name="ADSt 1949/51 M",   baseYear=1869, qslot="ADSt.1949.51.M");
mort.DE.census.1960.62.male = censtable(de.vz.dataBRD, name="ADSt 1960/62 M",   baseYear=1869, qslot="ADSt.1960.62.M");
mort.DE.census.1970.72.male = censtable(de.vz.dataBRD, name="ADSt 1970/72 M",   baseYear=1869, qslot="ADSt.1970.72.M");
mort.DE.census.1986.88.male = censtable(de.vz.dataBRD, name="ADSt 1986/88 M",   baseYear=1869, qslot="ADSt.1986.88.M");

mort.DE.census.1871.81.female = censtable(de.vz.dataDR, name="ADSt 1871/81 F",   baseYear=1869, qslot="ADSt.1871.81.F");
mort.DE.census.1881.90.female = censtable(de.vz.dataDR, name="ADSt 1881/90 F",   baseYear=1869, qslot="ADSt.1881.90.F");
mort.DE.census.1891.1900.female = censtable(de.vz.dataDR, name="ADSt 1891/1900 F",   baseYear=1869, qslot="ADSt.1891.1900.F");
mort.DE.census.1901.10.female = censtable(de.vz.dataDR, name="ADSt 1901/10 F",   baseYear=1869, qslot="ADSt.1901.10.F");
mort.DE.census.1910.11.female = censtable(de.vz.dataDR, name="ADSt 1910/11 F",   baseYear=1869, qslot="ADSt.1910.11.F");
mort.DE.census.1924.26.female = censtable(de.vz.dataDR, name="ADSt 1924/26 F",   baseYear=1869, qslot="ADSt.1924.26.F");
mort.DE.census.1932.34.female = censtable(de.vz.dataDR, name="ADSt 1932/34 F",   baseYear=1869, qslot="ADSt.1932.34.F");
mort.DE.census.1949.51.female = censtable(de.vz.dataBRD, name="ADSt 1949/51 F",   baseYear=1869, qslot="ADSt.1949.51.F");
mort.DE.census.1960.62.female = censtable(de.vz.dataBRD, name="ADSt 1960/62 F",   baseYear=1869, qslot="ADSt.1960.62.F");
mort.DE.census.1970.72.female = censtable(de.vz.dataBRD, name="ADSt 1970/72 F",   baseYear=1869, qslot="ADSt.1970.72.F");
mort.DE.census.1986.88.female = censtable(de.vz.dataBRD, name="ADSt 1986/88 F",   baseYear=1869, qslot="ADSt.1986.88.F");




mort.DE.census.ALL.male = MortalityTables::makeQxDataFrame(
    mort.DE.census.1871.81.male,
    mort.DE.census.1881.90.male,
    mort.DE.census.1891.1900.male,
    mort.DE.census.1901.10.male,
    mort.DE.census.1910.11.male,
    mort.DE.census.1924.26.male,
    mort.DE.census.1932.34.male,
    mort.DE.census.1949.51.male,
    mort.DE.census.1960.62.male,
    mort.DE.census.1970.72.male,
    mort.DE.census.1986.88.male
);


mort.DE.census.ALL.female = MortalityTables::makeQxDataFrame(
    mort.DE.census.1871.81.female,
    mort.DE.census.1881.90.female,
    mort.DE.census.1891.1900.female,
    mort.DE.census.1901.10.female,
    mort.DE.census.1910.11.female,
    mort.DE.census.1924.26.female,
    mort.DE.census.1932.34.female,
    mort.DE.census.1949.51.female,
    mort.DE.census.1960.62.female,
    mort.DE.census.1970.72.female,
    mort.DE.census.1986.88.female
);

rm(de.vz.dataDR, de.vz.dataBRD, censtable)

###############################################################################

# plot(mort.DE.census.ALL.male, title="Vergleich österreichische Sterbetafeln, Männer", legend.position=c(1,0))
# plot(mort.DE.census.ALL.female, title="Vergleich österreichische Sterbetafeln, Frauen", legend.position=c(1,0))

