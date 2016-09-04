stopifnot(require(methods), require(utils), require(MortalityTables)) # MortalityTable classes; new; Excel reader

mortalityTables.load("Germany_Endowments_*", wildcard=TRUE)
