library(tidyverse)
library(here)
library(fs)

# read files
fls <- dir_ls(path = "data/Paulas_files/Shareholder_classification/")

dat <- fls |> map(readxl::read_xlsx, sheet = 2) |> 
    map(janitor::clean_names)

shr_class <- dat |> 
    map(function(x) x |> dplyr::select(
        shareholder = nombre_empresa_alfabeto_latino, 
        class = nace_rev_2_principal_seccion)) |> 
    bind_rows()

shr_class |> pull(class) |> unique()

shr_class <- shr_class |> 
    mutate(shareholder = str_to_title(shareholder))

save(shr_class, file = "data/shr_class.RData")
