library(tidyverse)
library(fs)
library(tictoc)

fls <- dir_ls("data/vessels/")
fls

load(fls[1])
load("data/boats.Rda")
# Each file has features, and owners

head(features)

features[[1]] |> print(n=56)
features |> map(.f = nrow) |> unlist() |> table()
features[[1000]] |> print(n=120)

## Features to extract:
## LLI NO: Lloyds ID; IMO; Flag; LLI Vessel Type; Status; Reg. Owner; built; Name;
## MMSI; 

key_features <- c("LLI NO:", "IMO:", "Flag:", "LLI Vessel Type:", "Status:", 
                  "Reg. Owner:", "Built:", "Name:", 'MMSI:')


## Start for loop here
out <- list() # output list
tic()
for (i in seq_along(fls)){
    load(fls[i])
    ## select features
    features <- features |> 
        map(.f = function(x) if(!is.null(x)) filter(x, feature %in% key_features))
    # extract the null queries
    not_ok <- map(features, is.null) |> unlist()
    empty_df <- map(features, nrow) |> unlist() == 0
    tic()
    out[[i]]  <- features[!not_ok][!empty_df] |> #head() |> 
        map(.f = function(x) {
            x |> unique() |> # remove duplicated MMSI or IMOs
                ## remove the duplication of flag
                mutate(values = str_remove_all(
                    values, pattern = "Since: [:alnum:]* \\d{2} \\w{3} \\d{4}")) |>
                mutate(values = str_remove_all(
                    values, pattern = "Since: \\d{2} \\w{3} \\d{4}")) |> 
                mutate(values = str_remove_all(
                    values, pattern = "Since: In the [:alnum:]* of \\d{2} \\w{3} \\d{4}")) |> 
                unique() |> 
                # makes missing values explicit
                mutate(values = case_when(values == "" ~ NA, TRUE ~ values)) |> 
                pivot_wider(names_from = feature, values_from = values) |> 
                janitor::clean_names()
        }) |> bind_rows()
    print(glue::glue("File ", i, " completed, it took: \n"))
    toc() #24s
}
toc() # 396s | 6.7mins

out <- bind_rows(out)
out # 13 537 vessels with information




#### debugging code ####
# features[!not_ok][[6]] |> 
#     unique() |> # remove duplicated MMSI or IMOs
#     ## remove the duplication of flag
#     mutate(values = str_remove_all(
#         values, pattern = "Since: [:alnum:]* \\d{2} \\w{3} \\d{4}"))
# 
z |> map(function(x) x |> pull(lli_no) |> class()) |> unlist() |> is.character() |> all()
z[[215]]$flag


df_boats <- df_boats |> 
    rename(mmsi = boats) |> 
    left_join(
        out |> 
            mutate(reg_owner = case_when(
                reg_owner == "AIS Unknown" ~ NA,
                reg_owner == "Unknown Owners" ~ NA,
                TRUE ~ reg_owner
            ), mmsi = as.numeric(mmsi))
    )

save(df_boats, file = "data/boats_owners.Rda")

out |> 
    mutate(reg_owner = case_when(
        reg_owner == "AIS Unknown" ~ NA,
        reg_owner == "Unknown Owners" ~ NA,
        TRUE ~ reg_owner
    )) |> 
    filter(!is.na(reg_owner)) |> 
    group_by(reg_owner) |> 
    summarise(n = n()) |> 
    arrange(desc(n)) |> 
    print(n = 100)

out |> filter(!is.na(lli_vessel_type)) |> 
    pull(lli_vessel_type) |> 
    table()
