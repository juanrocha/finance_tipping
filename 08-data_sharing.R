library(tidyverse)


## financial actors
load("data/investors_cleaned.RData")

## Companies: initial scope
df1 <- readxl::read_xlsx(
    path = "data/Paulas_files/001-Drivers_Increasing_Risk_ED.xlsx",
    sheet = 2,
    na = "NA") %>% 
    janitor::clean_names()

readr::write_csv(
    dat, file = "data/investors_cleaned.csv"
)


readr::write_csv(
    df1, file = "data/companies.csv"
)
