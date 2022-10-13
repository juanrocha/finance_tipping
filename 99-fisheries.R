## Exploring fisheries data

library(tidyverse)

# ocean disclosure project:
odp <- readxl::read_excel("data/005-Ocean_disclosure_database.xlsx", sheet = 2) |> 
    janitor::clean_names()

odp |> skimr::skim()
    
