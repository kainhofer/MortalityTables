library(tidyverse)
library(openxlsx)
library(here)
library(readODS)
library(progress)
library(reshape2)

################################################################################
#  Helper Functions
################################################################################

StatAustria.readODSMortality = function(filename, sheet, jahr = sheet, oldFormat = FALSE, ...) {
# browser()
    # The tables provided by statistik Austria have duplicate column names (for
    # male, female and unisex data), so when selecting the corresponding columns,
    # we also need to rename them to contain the sex!
    tmp.tbl = read_ods(filename, sheet = sheet, skip = ifelse(oldFormat, 10, 5), .name_repair = "minimal")
    # alle Spalten mit q(x)
    qxname = ifelse(oldFormat, "q.x.", "qx")
    indices = which(colnames(tmp.tbl) %in% paste0(qxname, c("", ".1", "..1", ".2", "..2", ".3", "..3")))
    # bis 2002 nur M/F, ab 2002 auch unisex
    geschlecht = c("M", "F", "U")[1:length(indices)]
    names(indices) <- geschlecht
    # Extraktion Spalten mit q(x), Umbenennung, Transformation auf langes Format
    tmp.tbl %>%
        select(any_of(c(Alter = 1, indices))) %>%
        mutate(Jahr = jahr, Alter = suppressWarnings(as.numeric(Alter))) %>%
        filter(!is.na(Alter)) %>%
        pivot_longer(cols = geschlecht, names_to = "Geschlecht", values_to = "qx")
}



################################################################################
#  OFFICIAL Population Mortality Tables provided by Statistik Austria
################################################################################

# url.StT.official = "https://www.statistik.at/fileadmin/pages/413/Ausfuehrliche_allgemeine_und_ausgeglichene_Sterbetafeln_186871_bis_202022.ods"
# filename.StT.official   = here("data-raw", "Austria", basename(url.StT.official))
# download.file(url.StT.official, filename.StT.official, method = "curl")

# => Downloaded manually and pasted into Austria_Census.xlsx


################################################################################
#  YEARLY GRADUATED Population Mortality Tables (2016--2022) provided by Statistik Austria
################################################################################

url.StT.yearlyGrad = "https://www.statistik.at/fileadmin/pages/413/Geglaettete_Sterbetafeln_2016_2022.ods"
filename.StT.yearlyGrad   = here("data-raw", "Austria", basename(url.StT.yearlyGrad))
download.file(url.StT.yearlyGrad, filename.StT.yearlyGrad, method = "curl")

Jahre = list_ods_sheets(filename.StT.yearlyGrad)

pb = progress_bar$new(total = length(Jahre),
                      format = "   Lade Sterbetafeln  [:bar] :percent (:current/:total), eta :eta"
);
pb$tick(0);

df.qx = data.frame(Jahr = NA_character_, Alter = NA_integer_, Geschlecht = NA_character_, qx = NA_real_) %>%
    filter(!is.na(Alter))
for (jahr in Jahre) {
    # Worksheet laden und in globalem DF zwischenspeichern
    qx = StatAustria.readODSMortality(filename.StT.yearlyGrad, sheet = jahr)
    df.qx = bind_rows(df.qx, qx)
    pb$tick()
}

## Nach Excel und CSV rausschreiben (pro Geschlecht):
filename.StT.yearlyGradTable = file.path(dirname(filename.StT.yearlyGrad), paste0("Austria_JaehrlicheSterbetafeln_Geglättet_", str_sub(min(Jahre), 1, 4), "-", str_sub(max(Jahre), -4, -1), ".xlsx"))
wb <- createWorkbook()
# options(openxlsx.borderColour = "#4F80BD")
# options(openxlsx.borderStyle = "thin")
modifyBaseFont(wb, fontSize = 10, fontName = "Arial Narrow")

for (g in c("M", "F", "U")) {
    qx.data = df.qx %>%
        filter(Geschlecht == g) %>%
        acast(Alter ~ Jahr, value.var = "qx") %>%
        as_tibble(rownames = "Alter") %>%
        mutate(Alter = as.integer(Alter))
    addWorksheet(wb, sheetName = g, gridLines = FALSE)
    writeData(wb, sheet = g, startCol = 1, startRow = 1,
              x = paste0("Jährliche Sterbetafeln Österreich ", c("M"= "Männer", "F"="Frauen", "U" = "Unisex")[g], ", ", str_sub(min(Jahre), 1, 4), "-",str_sub(max(Jahre), -4, -1)))
    writeData(wb, sheet = g, startCol = 1, startRow = 2,
              x = "Quelle: Statistik Austria, https://www.statistik.at/statistiken/bevoelkerung-und-soziales/bevoelkerung/demographische-indikatoren-und-tafeln/sterbetafeln")
    writeDataTable(wb, sheet = g, startRow = 4, x = qx.data, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight9")
    freezePane(wb, sheet = g, firstActiveRow = 4, firstActiveCol = 2)
}

saveWorkbook(wb, filename.StT.yearlyGradTable, overwrite = TRUE)

df.qx %>%
    write.csv(file = file.path("inst", "extdata", paste0("Austria_Population_YearlyGraduated.csv")), row.names = FALSE)


# openXL(filename.StT.yearlyGradTable)




################################################################################
#  Yearly Population Mortality Tables (ungraduated) provided by Statistik Austria
################################################################################

## Download current population mortality tables and extract / convert to an easier-to-handle format
## Source: Statistik Austria, https://www.statistik.at/statistiken/bevoelkerung-und-soziales/bevoelkerung/demographische-indikatoren-und-tafeln/sterbetafeln
## Data: * graduated census tables (1868/71 - 2020/22), roughly every 10 years
##       * yearly ungraduated / raw population mortality tables (1947 - 2022), yearly; From 2002 on also unisex



# yearly mortality tables 1947--2022
#  1) Lade jede der TAbellen, extrahiere q(x) für M/F/U (U ab 2002)
#  2) Transformiere in data.frame (long format) mit Spalten Jahr, Geschlecht, Alter, qx
#  3) füge Daten zum globalen data.frame df.qx hinzu
#  4) Nach dem Laden: Erzeuge pivot-Tabelle für M/F/U und schreibe sie in ein Excel bzw. csv
url.StT.yearly   = "https://www.statistik.at/fileadmin/pages/413/Jaehrliche_Sterbetafeln_1947_bis_2022_fuer_Oesterreich.ods"
filename.StT.yearly   = here("data-raw", "Austria", basename(url.StT.yearly))
download.file(url.StT.yearly, filename.StT.yearly, method = "curl")
Jahre = list_ods_sheets(filename.StT.yearly)


pb = progress_bar$new(total = length(Jahre),
        format = "   Lade Sterbetafeln  [:bar] :percent (:current/:total), eta :eta"
    );
pb$tick(0);


df.qx = data.frame(Jahr = NA_integer_, Alter = NA_integer_, Geschlecht = NA_character_, qx = NA_real_)
for (jahr in Jahre) {
    # Worksheet laden und in globalem DF zwischenspeichern
    qx = StatAustria.readODSMortality(filename.StT.yearly, sheet = jahr, jahr = as.numeric(jahr), oldFormat = jahr<2002)
    df.qx = bind_rows(df.qx, qx)
    pb$tick()
}

## Nach Excel und CSV rausschreiben (pro Geschlecht):
filename.StT.yearlyTable = file.path(dirname(filename.StT.yearly), paste0("Austria_JaehrlicheSterbetafeln_", min(Jahre), "-", max(Jahre), ".xlsx"))
wb <- createWorkbook()
# options(openxlsx.borderColour = "#4F80BD")
# options(openxlsx.borderStyle = "thin")
modifyBaseFont(wb, fontSize = 10, fontName = "Arial Narrow")

for (g in c("M", "F", "U")) {
    qx.data = df.qx %>%
        filter(Geschlecht == g) %>%
        acast(Alter ~ Jahr, value.var = "qx") %>%
        as_tibble(rownames = "Alter") %>%
        mutate(Alter = as.integer(Alter))
    qx.data %>%
        write.csv(file = file.path("inst", "extdata", paste0("Austria_Population_Observation_", g, ".csv")), row.names = FALSE)
    addWorksheet(wb, sheetName = g, gridLines = FALSE)
    writeData(wb, sheet = g, startCol = 1, startRow = 1,
              x = paste0("Jährliche Sterbetafeln Österreich ", c("M"= "Männer", "F"="Frauen", "U" = "Unisex")[g], ", ", min(Jahre), "-", max(Jahre)))
    writeData(wb, sheet = g, startCol = 1, startRow = 2,
              x = "Quelle: Statistik Austria, https://www.statistik.at/statistiken/bevoelkerung-und-soziales/bevoelkerung/demographische-indikatoren-und-tafeln/sterbetafeln")
    writeDataTable(wb, sheet = g, startRow = 4, x = qx.data, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleLight9")
    freezePane(wb, sheet = g, firstActiveRow = 4, firstActiveCol = 2)
}

saveWorkbook(wb, filename.StT.yearlyTable, overwrite = TRUE)

openXL(filename.StT.yearlyTable)


