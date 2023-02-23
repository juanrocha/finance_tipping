library(tidyverse)
library(heatwaveR)
library(fs)
library(here)
library(raster)
library(tictoc)

# Analyze the sea surface temperature data to identify places where they can happen

fls <- dir_ls("~/Documents/Projects/DATA/OISST/")
# a dataframe with the dates will make easier to filter out 
df_fls <- tibble(file = fls)
df_fls <- df_fls |> 
    mutate(date = file |>
               str_remove("/Users/juanrocha/Documents/Projects/DATA/OISST/oisst-avhrr-v02r01.")) |> 
    mutate(date = str_remove(date, ".nc"))


# one file: can be use to extract relevant coordinates
dat <- raster(fls |> str_subset("20071022"), varname = "sst") # only one file
dat

## create a mask without NAs
coords <- as.data.frame(dat, xy = TRUE) |> 
    as_tibble()|> 
    filter(!is.na(Daily.sea.surface.temperature)) |> 
    dplyr::rename(lon = x, lat = y, sst = Daily.sea.surface.temperature)
# creates a unique id of the pixels that need to be studied
coords <- coords |> 
    unite(col = "id", lon, lat, sep = "_", remove = FALSE)

## Multiple files: one month - test the statistics.
tic()
stk <- stack(fls |> str_subset("202212"), quick = TRUE, varname = "sst")
toc() # 0.6s for one month,

stk_df <- as.data.frame(stk, xy = TRUE) |> 
    as_tibble() # 261MB World


tic()
stk_df |> 
    pivot_longer(cols = starts_with("Daily"), names_to = "day", values_to = "temp") |> 
    mutate(day = str_remove(day, "Daily.sea.surface.temperature.")) |> 
    group_by(x, y) |> 
    filter(all(is.na(temp)))
toc() ## takes too long, rather pre-compute pixels that should not be used.

raster::plot(dat)




#### leftover ####
# example code to analyze one ts

head(heatwaveR::sst_WA)
ts <- ts2clm(sst_WA, climatologyPeriod = c("1982-01-01", "2011-12-31"))
mhw <- detect_event(ts, categories = TRUE, climatology = TRUE)
mhw$event |> names()

mhw$climatology |> dplyr::filter(!is.na(category))
mhw$event |> 
    dplyr::select(category) |> table()
