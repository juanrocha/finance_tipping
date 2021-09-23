# A script to collect coordinates and geographical units that help combine datasets
# within the project
# Juan Rocha
# 20210914
library(tidyverse)
library(ggmap)

# register key for the session:
# register_google(key = "<insert key here>") # available on your google console

# import data:
df1 <- readxl::read_xlsx(
    path = "data/Paulas_files/001-Drivers_Increasing_Risk_ED.xlsx",
    sheet = 2,
    na = "NA") %>% 
    janitor::clean_names()

df1 <- df1 %>% 
    mutate(place = case_when(
        !is.na(subregion) & !is.na(country) ~ str_c(subregion, country, sep = ", "),
        !is.na(subregion) & is.na(country) ~ str_c(subregion, origin, sep = ", "),
        is.na(subregion) & !is.na(country) ~ country ,
        is.na(subregion) & is.na(country) ~ origin
    )) 

# extract the unique locations

dat <- select(df1, place) %>% unique()

# interact with GoogleMaps API to recover coordinates:

res <- dat %>% 
    mutate_geocode(location = place)

## Some records resulted in errors:
probs <- res %>%  filter(is.na(lon))

probs <- probs %>% 
    mutate(place = case_when(
        place == "Southeastern region, China" ~ "Guangzhou, China", 
        place == "Mato Grosso, Bolivia" ~ "Mato Grosso, Brazil",
        place == "South region, Congo" ~ "Kinshasa, Congo",
        place == "Central region, Argentina" ~ "Cordoba, Argentina",
        place == "Southeastern region, Gabon" ~ "Ogooué-Maritime, Gabon",
    ))

res2 <- probs %>% 
    mutate_geocode(location = place)

res <- res2 %>% 
    rename(lon = `lon...4`, lat = `lat...5`) %>% 
    select(place, lon, lat) %>% 
    bind_rows(res)

save(res, file = "data/coords.RData")


## TODO: now all coords are calculated, I need to correct and standardize names 
## on the dataset to be able to merge them.