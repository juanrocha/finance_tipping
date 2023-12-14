# many of the companies identified in Lloys do not match a company in Orbis
# probably due to different spellings on both databases. Here we attempt to 
# match companies with alternative names

library(tidyverse)


load("data/boats_owners.Rda")
load( "data/cleaned_data_230708.Rda")

guos
owners
revs

# 000 means the company is dissolved
guos_missing <- guos |> 
    filter(is.na(orbis_id)) |> 
    mutate(company = str_remove_all(
        company, "S.A.|Company|Limited|LLC|H/f|AS|Incorporated|Inc")) |> 
    mutate(company = str_trim(company, "both")) |> 
    pull(company)

### Now with gous_missing run 15-shareholders.R again


#### Compare with Carmine's dataset ####
car_dat <- read_csv(file = "~/Documents/Projects/high-seas-corporations/data/03_output/HS_fishing_vessels_with_corp_actors.csv")
car_dat <- readxl::read_xlsx(path = "~/Documents/Projects/high-seas-corporations/data/02_processed/corp_info_HS_fishing_vessels.xlsx", sheet = 1)

car_dat |> names()

car_dat |> 
    select(year, MMSI, best_flag, gfw_owner, owner:last_col())



missing_mmsi <- df_boats |> 
    filter(is.na(reg_owner)) |> 
    pull(mmsi)

(missing_mmsi %in% car_dat$MMSI) |> sum() # 2244
(df_boats |> filter(!is.na(reg_owner)) |> pull(mmsi) %in% car_dat$MMSI) |> sum() #840

# filter only boats that appear on our dataset of vessels fishing on risk areas
car_dat <- car_dat |> 
    filter(MMSI %in% missing_mmsi) |> 
    select(MMSI, owner, gfw_owner)

### Now with car_dat run 15-shareholders.R again
car_owners <- car_dat |> 
    select(owner) |> 
    unique() |> 
    filter(owner != 'n/a') |> 
    mutate(owner = str_remove(owner, pattern = "\\(.*")) |> 
    mutate(owner = str_remove(
        owner, pattern = "LTD|LTDA|IMP|EXP|LLC|S\\.L\\.|YK$|\\.\\,|\\.\\, \\.|\\,$|\\, \\.$")) |>
    mutate(owner = str_trim(owner, "both")) |> unique() |> #print(n=100)
    pull(owner) 

car_dat |> 
    filter(str_detect(owner, "OKAY"))
