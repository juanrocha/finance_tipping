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


df_shp <- map(files, read_sf)

rbind(df_shp[[1]], df_shp[[3]]) ## it works

df_all <- map_df(df_shp, rbind)

df_all <- left_join(df_all, dat, by = c("SppName" = "eid_name"))

df_all %>% 
  ggplot() + 
  geom_sf(aes(fill = path_disease), alpha = 0.5, size = 0.4) + 
  xlim(-180,180) +
  labs(title = "Observations of emergent zoonotic diseases", 
       subtitle = "149 dieseases recorded from 1970 to 2003",
       caption = "Data source: Allen et al 2017") +
  scale_fill_discrete( 
    guide = guide_legend(
      title = "Diseases", title.position = "top", title.hjust = 0.5,
      ncol = 3, keywidth = 0.5, keyheight = 0.5
    )) +
  theme_light(base_size = 7) + theme(legend.position = "bottom")

ggsave(
  filename = "Allen_data.png",
  plot = last_plot(),
  device = "png", 
  path = "figures/",
  width = 7, height = 4, dpi = 400,
  bg = "white"
)

## World maps for comparison:
## This world map is over ~2Gb, filter only level1 admin unit 
gam <- read_sf("~/Documents/Projects/DATA/GADM_maps/gadm36_shp/gadm36.shp")

gam %>% 
  select(starts_with("NAME")) %>% 
  filter(is.na(NAME_2)) %>% 
    ggplot() +
    geom_sf()
