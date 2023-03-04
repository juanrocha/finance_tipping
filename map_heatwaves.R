library(tidyverse)
library(fs)
library(here)
library(tictoc)


fls <- dir_ls("data/processed_MHW/")

## function to summarize each file
mhw_summary <- function(file){
    dat <- read_csv(file)
    x <- dat |> 
        group_by(lon, lat) |> 
        summarize(
            mean_intensity = mean(intensity_mean),
            n_events = n(),
            tot_days = sum(duration))
    return(x)
    rm(dat, x)
}
# create a function that slice only one year to check if results align with other papers
mhw_year <- function(file, year){
    dat 
}

mhw_categories <- function(file){
    dat <- read_csv(file)
    x <- dat |> 
        group_by(lon, lat, category) |>
        add_column(n = 1) |> 
        summarize(tot = sum(n)) |> 
        pivot_wider(names_from = category, values_from = tot)
    return(x)
    rm(dat, x)
}
# create a function that extracts the proportion of the events above / below the 
# categories thresholds.
dat <- read_csv(fls[1])


tic()
out <- map(fls, mhw_summary)
toc() # 1.3s

out <- out |>
    bind_rows()
# 
# out |> 
#     pivot_longer(cols = starts_with("I"), names_to = "category", values_to = "events" ) |> 
#     filter(category != "I Moderate") |> 
#     ggplot(aes(lon, lat)) +
#     geom_tile(aes(fill = events)) +
#     scale_fill_viridis_c(option = "D") + 
#     facet_wrap(~category) +
#     theme_void(base_size = 6) +
#     theme(legend.position = "bottom")

out |> ggplot(aes(mean_intensity)) + geom_density()
out$mean_intensity |> summary()

out |> 
    ungroup() |> 
    ggplot(aes(lon, lat)) + 
    geom_tile(aes(fill = mean_intensity, alpha = (mean_intensity > 1.45))) +
    #geom_tile(aes(fill = mean_intensity, alpha = n_events)) +
    scale_fill_viridis_c( option = "D") + 
    theme_void(base_size = 6) +
    theme(legend.position = "bottom")
    

# I forgot to calculate the severity categories on `heatwaves.R`. It takes over 10hrs
# to compute, so only do it if reviewers ask for it.