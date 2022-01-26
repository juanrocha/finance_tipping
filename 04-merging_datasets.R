library(tidyverse)

# There are three data levels to merge:
# 1. case studies (df0 from networks script. Regions and commodities selected N=9, I need country / case)
# 2. Companies (df1 from networks script script)
# 3. Financial actors (dat from 02 script)

## Companies
df1 <- readxl::read_xlsx(
    path = "data/Paulas_files/001-Drivers_Increasing_Risk_ED.xlsx",
    sheet = 2,
    na = "NA") %>% 
    janitor::clean_names()
## case studies: commodities in key regions (n = 9, table at: https://docs.google.com/presentation/d/1Cj84lwBhwMw0HJvPtyZz9pZMCGGU2OvR9GVXcMUbvbE/edit?usp=sharing)
df0 <- readxl::read_xlsx(
    path = "data/Paulas_files/001-Drivers_Increasing_Risk_ED.xlsx",
    sheet = 1,
    na = "NA") %>%
    janitor::clean_names()
## J211207: this codification of case studies is not consistent with what is to be presented in the paper. Alternatively I'm coding down by hand.

## financial actors
load("data/investors_cleaned.RData")

## Adding case level to the data:
df0 #not useful, it is a preliminary file with not consistent coding
df1 # is the dataset to construct the networks, it has companies, where are based and what they exploit
dat # is the cleaned dataset of companies
dfx # file cleaned by Paula, I belive it mixes companies and sharehodlers, it has more levels on companies than the raw data.

## to make companies and financial actors compatible:
df1 <- df1 |> 
    mutate(
        company = str_to_title(company),
        country = ifelse(is.na(country), origin, country))

### Adding case information based on google slide:
### Case1
df1 <- df1 |> 
    mutate(casestudy = case_when(
            country %in% c("Indonesia", "Myanmar", "Malaysia",  "Thailand" , "Cambodia"  ) & 
                commodity == "Palm oil" ~ "1-Southeast Asia",
            country %in% c("Indonesia", "Malaysia", "Thailand", "Cambodia","Vietnam","Philippines") & 
                commodity == "Pulp and wood products" ~ "2-Southeast Asia",
            country %in% "USA" & 
                commodity == "Pulp and wood products" ~ "3-North America",
            country %in% c("Brazil","Argentina", "Paraguay",  "Uruguay") & 
                commodity == "Soybeans" ~ "4-South America",
            country %in% c("Japan", "China", "South Korea")& 
                commodity == "Pulp and wood products" ~ "5-East Asia",
            country %in% (df0 |> filter(str_detect(region, "Europe")) |> 
                              filter(!country %in% 
                                         c("Crotia", "Bosnia", "Serbia", "Belarus", "Turkey",
                "Netherlands", "Slovenia", "Montenegro", "Albania", "Macedonia", "Kosovo", "Denmark",
                "Finland", "Sweden", "Iceland", "Norway", "Ireland", "Latvia", "Grece", "Switzerland",
                "Italia")) |> 
                              pull(country) |> unique()) & 
                commodity == "Pulp and wood products" ~ "6-Europe",
            country %in% c("Cameroon","Ivory Coast", "Ethiopia","Ghana","Congo", "Uganda", "Nigeria", "Sierra Leone", "Madagascar", "Tanzania") & 
                commodity == "Cocoa" ~ "7-Africa",
            country %in%  c("Cameroon","Ivory Coast", "Ghana","Congo", "Uganda", "Nigeria", "Sierra Leone", "Liberia", "South Africa", "Gabon", "Republic of Sao Tome") &
                commodity == "Palm oil" ~ "8-Africa",
            country %in%  c("Brazil","Argentina", "Paraguay", "Uruguay")  & 
                commodity == "Cattle"  ~ "9-South America",
            TRUE ~ "Other"
        )
    )


skimr::skim(df1)

df1 |> pull(casestudy) |> table()

df1 |> select(casestudy, origin, country, commodity, company)

save(df1, df0, file = "data/casestudies.RData")
