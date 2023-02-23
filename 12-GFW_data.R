## Exploring the global fish watch data

library(tidyverse)


dat <- read_csv("~/Documents/Projects/DATA/GlobalFishWatch/9c3d6bc0-a3c1-11ed-a61d-7ba19957371b/fishing-vessels-v2.csv")

dat |> names()
dat |> skimr::skim()
dat |> pull(registries_listed) |> unique()
dat |> pull(vessel_class_registry) |> unique() ## identifies tuna as target
dat

