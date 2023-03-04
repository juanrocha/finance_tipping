library(tidyverse)
library(heatwaveR)
library(fs)
library(here)
library(tidync)
library(tictoc)
library(doParallel)
registerDoParallel(cores = 10)
#library(data.table)

#### Heatwaves on big ncdf file ####
# J230301: `processing_sst.Rmd` was used to combine all .nc files from NOAA into
# a unique file that can be queried. Instead of reading files multiple times one
# reads the big one once and call slices at the time.
# Following code from the package vignette also available at:
# https://robwschlegel.github.io/heatwaveR/articles/Download_SST_v2.html

nc_file <- dir_ls("data/") |> str_subset("combined.nc") # one file
datadir <- "data/processed_MHW" # processed marine heatwaves

## Function to read data (from tutorial)
OISST_load <- function(file_name, lon1, lon2) {
    OISST_dat <- tidync(file_name) %>%
        hyper_filter(lon = between(lon, lon1, lon2)) %>%
        hyper_tibble(select_var = "sst", force = TRUE, drop = TRUE) %>%
        dplyr::select(-zlev) %>%
        dplyr::rename(t = time, temp = sst) %>%
        mutate(t = as.Date(t, origin = "1978-01-01"))
    return(OISST_dat)
    rm(OISST_dat)
}

# Event detection function (from tutorial)
event_only <- function(df) {
    # first calculate the climatologies
    clim <- ts2clm(data = df, climatologyPeriod = c("1989-01-01", "2019-12-31"))
    # then the events
    event <- detect_event(data = clim, categories = TRUE, climatology = TRUE)
    rm(clim)
    # return only the event metric dataframe of results
    return(event$event)
    rm(event)
}

# Define the slices
# 5° longitude slices seem to work fine on
# my MacBook Pro with 32Gb RAM and 12 cores
slice_df <- tibble(lon1 = seq(0, 355, 5),
                   lon2 = seq(5, 360, 5))

# test on one slide
# tic()
# sst <- OISST_load(nc_file, lon1 = slice_df$lon1[1], lon2 = slice_df$lon2[1]) |> 
#     filter(t < "2020-02-01") ## added line to address problem below
# toc() # 60s, including filter 74s

## I get an error with the following:
## Error in do.ply(i) :
## task 1 failed - "The specified end date follows the last day of series, which is 2020-02-27"
## It suggest some pixels do not have complete time series. In fact checking the files, some of
## them after 2022 have 1.5Mb instead of 1.6Mb. They are either incomplete, or corrupt. I did have
## the same problem before and re-downloaded manually files without the expected size (much smaller).
## There are currently 416 files with such problem, most of it 2022. Checking back in NOAA the files do
## indeed have smaller sizes. A solution is to omit the last part of the time series on the analysis.


# tic()
# MHW <- plyr::ddply(.data = sst, .variables = c("lon", "lat"),
#                    .fun = event_only, .parallel = TRUE)
# toc() # 70s

## estimate of 145s per slide * 72 slices = 3hrs estimated computation time.
## If errors continue, perhaps recode the for loop as a purrr::map equivalent + safely

tic()
system.time(
    # extract slices sequentially
    for (i in 1:nrow(slice_df)) {
        tic()
        cat(noquote(paste("Processing slice", i, "of", nrow(slice_df),
                          "-->", slice_df$lon1[i], "to", slice_df$lon2[i], "°E\n")))
        cat(noquote("  > 1. loading and slicing NetCDF\n"))
        sst <- OISST_load(nc_file, lon1 = slice_df$lon1[i], lon2 = slice_df$lon2[i])
        # process each slice in parallel
        cat(noquote("  > 2. detecting marine heatwaves\n"))
        MHW <- plyr::ddply(.data = sst, .variables = c("lon", "lat"),
                           .fun = event_only, .parallel = TRUE)
        rm(sst)
        # save results to disk
        cat(noquote("  > 3. saving events to csv\n"))
        data.table::fwrite(MHW, file = paste0(datadir, "/MHW_slice_", i, "_",
                                  slice_df$lon1[i], "-", slice_df$lon2[i], ".csv"),
                           showProgress = TRUE)
        rm(MHW)
        cat(noquote("SLICE DONE!\n"))
        toc()
        cat(sep="\n\n")
    }
)
toc() #38993.427 s = 10.8hrs; 14.6hrs with event category detection


## Results are saved on folder data/processed_MHW in csv files per slice.
## Process them and create a Rda file with a map of the marine heatwaves at 0.25deg





#### heatwaves on individual files ####
# Initial code to explore data. Then I used `processing_sst.Rmd` to create a unique data cube
# on a .nc file trying to speed up computations. 
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
