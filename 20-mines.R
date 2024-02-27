library(tidyverse)
library(sf)

dat <- read_sf(dsn = "data/mines_sp_old.gpkg")


dat |> 
    ggplot() +
    geom_sf()

dat # 34 820 obs

## Option 1: download all mines and figure out intersecitons with resilience loss
## or loss of functional integrity later.
## Opetion 2: do the intersection first and only download the mines of interest.
## If Option 1, we can also study a bit the issue of concentration.

dat |> pull(mine) |> unique() |> length() # 29 718 mines
dat |> pull(known_as) |> unique() |> length() # 18 268 aka.

## reduce dataset to unique mines:
df_dat <- dat |> 
    as_tibble() |> 
    select(mine, known_as) |> 
    unique() 

df_dat |> print(n=200)
df_dat <- df_dat |> filter(!is.na(mine))
mines <- df_dat |>  pull(mine) |> unique() 

#### Orbis ####

# Orbis script run, data object:
load("data/mining_shareholders.RData")

#### Explore shareholders ####

guo_list[[1]]
