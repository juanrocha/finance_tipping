library(tidyverse)
library(fs)
library(here)


## Not very informative
dat <- read_csv(file = "/Users/juanrocha/Downloads/Nearshore Fish Atlas of Alaska_ID_17274.csv")
dat |> 
    names()

dat <- read_csv("/Users/juanrocha/Downloads/goa2015_2019.csv")

## from Alaskan pollock permits:
## files ending in cp are catcher processors, endin gin cv are catcher vessels
fls <- dir_ls("data/Alaska_pollock/")

dat <- fls |> str_subset("afa") |> 
    map(.f = function(x) read_csv(x, skip=1) |> janitor::clean_names()) |> 
    map(select, owner_name) |> 
    bind_rows() |> 
    unique()
dat

read_csv("data/fishing_companies_alaskan-pollock.csv")

