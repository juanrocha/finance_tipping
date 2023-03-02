library(tidyverse)
library(fs)
library(here)
library(tictoc)


fls <- dir_ls("data/processed_MHW/")

## function to summarize each file
mhw_summary <- function(file){
    tic()
    dat <- read_csv(file)
    x <- dat |> 
        group_by(lon, lat) |> 
        summarize(mean_intensity = mean(intensity_mean))
    return(x)
    rm(dat, x)
    toc()
}


out <- map(fls, mhw_summary)

out <- out |>
    bind_rows()

out |> 
    ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = mean_intensity)) +
    scale_fill_viridis_c("Mean intensity") + 
    theme_void(base_size = 6) +
    theme(legend.position = "bottom")
    

# I forgot to calculate the severity categories on `heatwaves.R`. It takes over 10hrs
# to compute, so only do it if reviewers ask for it.