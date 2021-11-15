## exploring World Governance Index dataset from the World Bank
## Juan Rocha
## 211108
## Source: https://datacatalog.worldbank.org/search/dataset/0038026/Worldwide-Governance-Indicators
## Background info: http://info.worldbank.org/governance/wgi/

library(tidyverse)

wgi <- read_csv(file = "~/Documents/Projects/DATA/WorldBank/WGI_csv/WGIData.csv") |> 
    janitor::clean_names()

wgi |> 
    pull(indicator_name) |> 
    unique()


## Some notes:
## Estimate	Estimate of governance (ranges from approximately -2.5 (weak) to 2.5 (strong) governance performance)
# StdErr	Standard error reflects variability around the point estimate of governance.
# NumSrc	Number of data sources on which estimate is based
# Rank	Percentile rank among all countries (ranges from 0 (lowest) to 100 (highest) rank)
# Lower	Lower bound of 90% confidence interval for governance, in percentile rank terms
# Upper	Upper bound of 90% confidence interval for governance, in percentile rank terms

## Work only with estimates for now:

wgi <- wgi |> 
    filter(str_detect(indicator_name, "Estimate")) |> 
    select(-x27) |> 
    pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "value") |> 
    mutate(year = str_remove(year, "x")) |> 
    mutate(year = as.numeric(year))


wgi |> 
    ggplot(aes(year, value, group = country_code)) +
    geom_path(alpha = 0.2, size = 0.5) +
    facet_wrap(~indicator_name)

df_wgi <- wgi |> 
    group_by(country_name, indicator_name) |> 
    summarize(estimate = mean(value, na.rm = TRUE)) |> 
    mutate(indicator_name = str_remove_all(indicator_name, ": Estimate"))

save(df_wgi, file = "data/governance_index_cleaned.RData")
