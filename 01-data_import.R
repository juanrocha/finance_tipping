library(tidyverse)

df1 <- readxl::read_xlsx(
    path = "data/Drivers_Increasing_Risk_ED-2.xlsx",
    sheet = 1) %>% 
    janitor::clean_names()

df2 <- readxl::read_xlsx(
    path = "data/Drivers_Increasing_Risk_ED-2.xlsx",
    sheet = 2,
    na = "n.d") %>% 
    janitor::clean_names()

df1 %>% skimr::skim()

df1 %>%
    ggplot(aes(country)) + 
    geom_bar() +
    coord_flip()
    
df1 %>%
    filter(country == "USA") %>% 
    pull(driver_of_change) %>% 
    unique()

df2 %>% # how far is it coded
    filter(!is.na(access_date)) %>% 
    skimr::skim()

## Fix missing values
df2 <- df2 %>% 
    mutate(
        across(where(is.character), ~str_remove(., pattern = "n.d."))) %>% 
    mutate(
        percent_certificated_areas = as.numeric(percent_certificated_areas) ,
        area_ha, no_workers =  as.numeric(area_ha),
        turnover_mll_usd = as.numeric(turnover_mll_usd))

df2 
