library(tidyverse)


load("data/bolivia_orbis.RData")
dat <- read_csv("data/fishing_companies_alaskan-pollock.csv")

dat

## Recovering IDs
guo_list[[1]][1][[1]][2,1]

dat$ids <-  guo_list |> 
    map(function(x) x[[1]][[1]][2]) |> 
    map(function(x) str_remove_all(x,pattern = "\n|\t|Orbis ID: ")) |> 
    map(function(x) str_remove_all(x, "ActiveBvD ID: |Active \\(dormant\\)BvD ID: "))

dat <- dat |> 
    unnest(ids, keep_empty = TRUE) |> 
    mutate(ids = str_replace(ids, "[:space:]", "_")) |> 
    separate(col = ids, into = c("BvD_id", "orbis_id"), sep = "_")

## We also have company address if needed
## Recover GUO:

guo_list[[1]][[1]][2,3]

dat$guo <- guo_list |> 
    map(function(x) x[[1]][2,3]) |> 
    # recover GUOs
    map(function(x) str_remove_all(x, pattern = "Private The Global Ultimate Owner of this controlled subsidiary is ")) |> 
    map(function(x) str_replace(x, "Private This company is the Global Ultimate Owner of the corporate group", "self-GUO")) |> 
    map(function(x) str_replace_all(x, "Private This company is Independent \\(but not the Global Ultimate Owner of a Corporate Group\\)", "independent")) |> 
    map(function(x) str_replace_all(x, "Private This company is a Single location", "single")) |> 
    map(function(x) str_remove_all(x, "Private This entity is a branch of "))

dat <- dat |> 
    unnest(guo, keep_empty = TRUE)

## Recover revenues
revenues[[1]][[1]][1,2] # units
revenues[[1]][[1]][3,2] # revenue
revenues[[1]][[1]][4,2] # employees

dat$rev_units <- revenues |> map(function(x) x[[1]][2][[1]][1])
dat$revenue <- revenues |> map(function(x) x[[1]][2][[1]][3])
dat$employees <- revenues |> map(function(x) x[[1]][2][[1]][4])

dat <- dat |> 
    unnest(cols = rev_units:employees, keep_empty = TRUE) |> 
    mutate(across(revenue:employees, .f = str_remove_all, pattern = ",")) |> 
    mutate(across(revenue:employees, .f = as.numeric))

## Shareholers
shr_list[[1]][[1]][1]
dat$shr_info <- shr_list |> 
    map(function(x) x[[1]][[1]][[1]]) |> 
    map(function(x) str_remove_all(x, "\n|\t")) |> 
    map(function(x) str_replace(x, "Name", "available"))

dat <- dat |> unnest(shr_info, keep_empty = TRUE)

idx <- which(dat$shr_info == "available")
shr_dat <- shr_list[idx] |> 
    map(function(x) x[[1]]) |> 
    map(function(x) {
        x |>  rename(
            name0 = X1, shr_name = X2, name2 = X3, country = X4, type = X5, ownership_direct = X6,
            ownership_total = X7, info_source = X8, info_date = X9, op_revenue_MUSD = X10, 
            empolyees = X11) |> 
            slice(3:(n()-1)) |> 
            select(-name0, -name2)
    }) |> 
    map2(dat$company[idx],function(x,y) {x$company <- y; return(x)}) |> 
    bind_rows()

shr_dat |>  
    filter(shr_name != company) |> 
    #select(company, everything())
    mutate(info_date = lubridate::my(info_date)) |> 
    group_by(company) |> 
    summarize(num_shr =n()) |> arrange(desc(num_shr))
 
write_csv(shr_dat, file = "data/Bolivia_shareholders.csv")
write_csv(dat, file = "data/Bolivia_companies.csv")
