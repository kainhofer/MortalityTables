library(usethis)
library(readxl)
library(here)

PopulationData.AT2017 = read_excel(here("data-raw", "Austria_Population2017_HMD_StatistikAustria.xlsx"), skip = 3)
usethis::use_data(PopulationData.AT2017)
