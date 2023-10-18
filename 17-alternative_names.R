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

