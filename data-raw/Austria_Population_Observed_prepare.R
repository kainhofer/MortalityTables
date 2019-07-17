library(tidyverse)
library(openxlsx)
filename = file.path("data-raw", "Austria_Population_Observed_StatistikAustria.xlsx")
wb = openxlsx::loadWorkbook(filename)

loadSheet = function(wb, sheet = "2017") {
    if (as.numeric(sheet) >= 2002) {
        startRow = 8
        cols = c(1,2,8,14)
        colNames = c("Alter", "M", "F", "U")
    } else {
        startRow = 13
        cols = c(1,2,8)
        colNames = c("Alter", "M", "F")
    }

    data = readWorkbook(wb, sheet = sheet, startRow = startRow, colNames = FALSE, rowNames = FALSE, cols = cols) %>%
        `colnames<-`(colNames) %>%
        filter(!is.na(M), !is.na(F)) %>%
        mutate(Alter = as.integer(Alter), Jahr = as.integer(sheet)) %>%
        gather(key = Geschlecht, value = qx, -Alter, -Jahr) %>%
        select(Jahr, Alter, Geschlecht, qx) %>%
        as.tibble

    data
}


qx.AT_Pop_observed = bind_rows(sapply(sheets(wb), loadSheet, wb = wb, simplify = FALSE))

for (g in c("M", "F", "U")) {
    qx.AT_Pop_observed %>%
        filter(Geschlecht == g) %>%
        acast(Alter ~ Jahr, value.var = "qx") %>%
        write.csv(file = file.path("inst", "extdata", paste0("Austria_Population_Observation_", g, ".csv")))
}


