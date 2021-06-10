library(tidyverse)
library(sf)
# Allen data

dat <- readxl::read_xlsx(
    path = "~/Documents/Projects/DATA/Allen_2005_DataMaps_Unprojected/0_Metadata_MOESM4_ESM.xlsx",
    sheet = 1, skip = 1
)

path <- "~/Documents/Projects/DATA/Allen_2005_DataMaps_Unprojected/"

files <- paste0(path, dat$filename)

length(files)

df_shp <- read_sf(files[1])

df_shp %>% names()
df_shp %>% 
    ggplot() + 
    geom_sf()
