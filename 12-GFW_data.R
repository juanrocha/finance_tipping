## Exploring the global fish watch data

library(tidyverse)
library(here)
library(fs)
library(sf)
library(raster)


dat <- read_csv("~/Documents/Projects/DATA/GlobalFishWatch/9c3d6bc0-a3c1-11ed-a61d-7ba19957371b/fishing-vessels-v2.csv")
load("data/mhw_exposure.Rda")
load("~/Documents/Projects/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_ChlorA_log.RData")

dat |> names()
dat |> skimr::skim()
dat |> pull(registries_listed) |> unique()
dat |> pull(vessel_class_registry) |> unique() ## identifies tuna as target
dat

## create boxes for each pixel. 
pxls <- pxls |> 
    mutate(xmin = lon - 0.125, xmax = lon + 0.125, ymin = lat - 0.125, ymax = lat + 0.125)
pxls

pxls |> 
    ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = mp_severe))

## Create a mask with pixels where boats IDs need to be recovered.
## they are in different projections

df_delta_detected |> 
    filter(n_ews >=3) |> 
    ggplot(aes(lon,lat)) +
    geom_tile(aes(fill=n_ews))

# pxls |> 
#     full_join(df_delta_detected |> 
#                   filter(n_ews >= 3) |> 
#                   select(lon, lat, n_ews))
## This is to get the original CRS from the NOAA SST data
fls <- dir_ls("~/Documents/Projects/DATA/OISST/")
sst <- raster(fls[1], varname = "sst")
crs(sst)
x <- st_crs("WGS84")

deltas <- rasterFromXYZ(df_delta_detected |> 
                            ungroup() |> 
                            dplyr::select(lon, lat, n_ews), 
                        crs = x$proj4string)

pxls2 <- rasterFromXYZ(pxls |> dplyr::select(lon,lat, mp_extreme, mp_severe),
                       crs = crs(sst)) |> 
    # rotate changes from 0-360 longitude to -180:180, compatible with other data
    rotate()

deltas <- extend(deltas, pxls2)

plot(deltas)
plot(pxls2$mp_extreme)

# mask with pixels where there is a non-zero prob of heatwaves or where at least
# 3 ews were detected
m <- sum((pxls2$mp_extreme > 0), (pxls2$mp_severe > 0), (deltas >= 3), na.rm = TRUE) > 0
plot(m)

## trying with as terra object
m <- terra::rast(m)
class(m)

# clean up memory
rm(deltas, pxls2, fls, x, sst, df_delta_detected)

## mmsi is the identifier of vessels. We need to subset vessels who fished in pixels
fls <- dir_ls("~/Documents/Projects/DATA/GlobalFishWatch/9c3d6bc0-a3c1-11ed-a61d-7ba19957371b/mmsi-daily-csvs-10-v2-2020/")
fls |> length()

boats <- map(
    fls,
    function(x) {
        d <- read_csv(x)|> 
            filter(fishing_hours > 0) |> 
            rename(x = cell_ll_lon, y = cell_ll_lat)
        d <- st_as_sf(d, coords = c("x", "y")) |> 
            st_set_crs(4326)
        pnts <- terra::extract(m, d)
        d$risk <- pnts$layer
        
        bts <- d |> 
            as_tibble() |> 
            filter(risk == TRUE) |> 
            pull(mmsi) |> 
            unique()
        return(bts)
    }
)

df_boats <- tibble(
    boats = unlist(boats)
) |> unique()


save(df_boats, file = "data/boats.Rda")

### Example with one file
# dat <- read_csv(fls[1])
# range(dat$cell_ll_lat)
# dat |> pull(mmsi) |> unique() |> length()
# 
# dat <- dat |> 
#     filter(fishing_hours > 0) |> 
#     rename(x = cell_ll_lon, y = cell_ll_lat) 
# 
# dat
# 
# dat <- st_as_sf(dat, coords = c("x", "y")) |> 
#     st_set_crs(4326)
# 
# dat |> 
#     ggplot(aes(lon, lat)) +
#     geom_point(aes(color = fishing_hours), size = 0.5) + 
#     scale_color_viridis_c()
# 
# class(m)
# 
# 
# pnts <- terra::extract(m, dat)
# 
# dat$risk <- pnts$layer
# 
# dat |> 
#     as_tibble() |> 
#     filter(risk == TRUE) |> 
#     pull(mmsi) |> 
#     unique()
