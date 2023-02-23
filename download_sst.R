library(tidyverse)
library(tictoc)
## Some of this code follows the steps of this tutorial to download NOAA SST data
## https://robwschlegel.github.io/heatwaveR/articles/OISST_preparation.html
## also available under the heatwaveR package


# First we tell R where the data are on the interwebs
OISST_base_url <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"
# Note that one may go to this URL in any web browser to manually inspect the files

# Now we create a data.frame that contains all of the dates we want to download
# NB: In order to change the dates download changes the dates in the following line
OISST_dates <- tibble(t = seq(as.Date("1981-09-01"), as.Date("2022-12-31"), by = "day"))

# To finish up this step we add some text to those dates so they match the OISST file names
OISST_files <- OISST_dates %>% 
    dplyr::mutate(t_day = base::gsub("-", "", t),
                  t_month = base::substr(t_day, 1, 6),
                  t_year = lubridate::year(t),
                  file_name = base::paste0(OISST_base_url, t_month, "/", "oisst-avhrr-v02r01.", t_day ,".nc"))

# This function will go about downloading each day of data as a NetCDF file
# Note that this will download files into a 'DATA/OISST' folder in the root directory
# If this folder does not exist it will create it
# If it does not automatically create the folder it will need to be done manually
# The folder that is created must be a new folder with no other files in it
# A possible bug with netCDF files in R is they won't load correctly from 
# existing folders with other file types in them
# This function will also check if the file has been previously downloaded
# If it has it will not download it again
OISST_url_daily_dl <- function(target_URL){
    base::dir.create("~/Documents/Projects/DATA/OISST", showWarnings = F)
    file_name <- base::paste0("~/Documents/Projects/DATA/OISST/",base::sapply(base::strsplit(target_URL, split = "/"), "[[", 10))
    if(!base::file.exists(file_name)) utils::download.file(url = target_URL, method = "libcurl", destfile = file_name)
}

# The more cores used, the faster the data may be downloaded
# It is best practice to not use all of the cores on one's machine
doParallel::registerDoParallel(cores = 10)

# And with that we are clear for take off
tic()
base::system.time(plyr::l_ply(OISST_files$file_name, .fun = OISST_url_daily_dl, .parallel = T))
toc() # ~15 seconds

# In roughly 15 seconds a user may have a full month of global data downloaded
# This scales well into years and decades, and is much faster with more cores
# Download speeds will also depend on the speed of the users internet connection