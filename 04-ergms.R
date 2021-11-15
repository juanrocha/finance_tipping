## exponential random graph models of financial actors
library(tidyverse)
library(sna)
library(ergm)
library(broom)

#### Load data ####
df1 <- readxl::read_xlsx(
    path = "data/Paulas_files/001-Drivers_Increasing_Risk_ED.xlsx",
    sheet = 2,
    na = "NA") %>% 
    janitor::clean_names()

#### Network of countries ###
# This first analysis merge all commodities into "geographies". Subsequent analysis 
# could separate by commodity and see if it holds
# Exclude nodes that are not countries:
no_country <- c("Central Europe", "Eastern Europe", "Middle East", "Southern Europe", "Europe", "Fejér")
net_countries <-  df1 %>% 
    select(company, origin, country) %>% 
    group_by(origin, country) %>% 
    # unique() %>% 
    count(name = "channel") %>% # a channel is a country * commodities
    filter(!is.na(origin), !is.na(country)) %>% 
    # Get rid of destinations that are not countries
    filter(!(origin %in% no_country), !(country %in% no_country)) |> 
    # remove "Liechtenstein" to include inequality metrics:
    filter(origin != "Liechtenstein" ) |> 
    ## standardize names to combine later with wgi dataset
    mutate(
        origin = case_when(
            origin == "Croacia" ~ "Croatia",
            origin == "Hong Kong" ~ "Hong Kong SAR, China" ,
            origin == "Ivory Coast" ~ "Cote d'Ivoire" ,
            origin == "New Zeland" ~ "New Zealand",
            origin == "Russia" ~ "Russian Federation",
            origin == "Yemen" ~ "Yemen, Rep.",
            origin == "South Korea" ~ "Korea, Rep." ,
            origin == "UK" ~ "United Kingdom"  ,
            origin == "USA" ~ "United States", 
            origin == "Slovakia" ~ "Slovak Republic" ,
            origin == "Congo" ~ "Congo, Rep.",
            origin == "Republic of Sao Tome" ~ "Sao Tome and Principe" ,
            origin == "Venezuela" ~ "Venezuela, RB" ,
            origin == "Laos" ~ "Lao PDR",
            origin == "Bosnia" ~ "Bosnia and Herzegovina" ,
            origin == "Egypt" ~ "Egypt, Arab Rep.", 
            origin == "Macedonia" ~ "North Macedonia" , 
            origin == "Republic of Congo" ~ "Congo, Rep.",
            TRUE ~ origin),
        country = case_when(
            country == "Croacia" ~ "Croatia",
            country == "Hong Kong" ~ "Hong Kong SAR, China" ,
            country == "Ivory Coast" ~ "Cote d'Ivoire" ,
            country == "New Zeland" ~ "New Zealand",
            country == "Russia" ~ "Russian Federation",
            country == "Yemen" ~ "Yemen, Rep.",
            country == "South Korea" ~ "Korea, Rep." ,
            country == "UK" ~ "United Kingdom"  ,
            country == "USA" ~ "United States", 
            country == "Slovakia" ~ "Slovak Republic" ,
            country == "Congo" ~ "Congo, Rep.",
            country == "Republic of Sao Tome" ~ "Sao Tome and Principe" ,
            country == "Venezuela" ~ "Venezuela, RB" ,
            country == "Laos" ~ "Lao PDR",
            country == "Bosnia" ~ "Bosnia and Herzegovina" ,
            country == "Egypt" ~ "Egypt, Arab Rep.", 
            country == "Macedonia" ~ "North Macedonia" , 
            country== "Republic of Congo" ~ "Congo, Rep.",
            TRUE ~ country
        )
    ) |>  
    network(directed = TRUE, bipartite = FALSE, matrix.type = "edgelist", ignore.eval=FALSE, loops = TRUE)


nodes <- network.vertex.names(net_countries)

## load the wgi
load("data/governance_index_cleaned.RData")
df_wgi <- df_wgi |> 
    pivot_wider(names_from = indicator_name, values_from = estimate) |> 
    filter(country_name %in% nodes)

c_name <- (df_wgi |> pull(country_name) |> unique())
nodes[nodes %in% c_name == FALSE] # 0 means all names are compatible!
# c_name[c_name %in% nodes == FALSE]

## load inequality data:
inq <- readxl::read_xlsx(
    path = "~/Documents/Projects/DATA/WIID_World_Income_Inequality_DB/WIID_06MAY2020.xlsx")
inq 


# here I pick all records after from 1990, average for the same year when more than
# one record is reported, and then average over time and rescale from 0-1
inq <- inq %>% 
    filter(!is.na(gini_reported), year >= 1990) %>% 
    select(country, isoa2 = c2, country_code = c3, year, gini_reported) %>% 
    group_by(country, year, isoa2, country_code) %>% 
    summarize(gini_mean = mean(gini_reported, na.rm = TRUE)) |> 
    ungroup() |> group_by(country, isoa2, country_code) |> 
    summarize(gini_mean = mean(gini_mean, na.rm = TRUE))

nodes[nodes %in% inq$country == FALSE]
inq$country[inq$country %in% nodes == FALSE]

## change country names to be able to merge datasets
inq <- inq |> 
    mutate(country = case_when(
        country == "Czechia"  ~ "Czech Republic",
        country == "Hong Kong" ~ "Hong Kong SAR, China",
        country == "Russia" ~ "Russian Federation",
        country == "Korea, Republic of" ~ "Korea, Rep." ,
        country == "Yemen" ~ "Yemen, Rep.",
        country == "Slovakia" ~"Slovak Republic" ,
        country == "Congo, Democratic Republic of the" ~ "Congo, Rep." ,
        country == "Venezuela" ~ "Venezuela, RB",
        country == "Laos" ~ "Lao PDR",
        country == "Egypt" ~ "Egypt, Arab Rep.",
        TRUE ~ country
    ))



nodes # note they are not alphabetical
# make sure the df is on the same order as nodes:
df_attr <- tibble(nodes = nodes) |> 
    left_join(df_wgi, by = c("nodes" = "country_name")) |> 
    left_join(inq, by = c("nodes" = "country") )


## add network attributes
net_countries %v% "corruption" <- df_attr$`Control of Corruption`
net_countries %v% "gov_effectiveness" <- df_attr$`Government Effectiveness`
net_countries %v% "pol_stability" <- df_attr$`Political Stability and Absence of Violence/Terrorism`
net_countries %v% "regulatory_qual" <- df_attr$`Regulatory Quality`
net_countries %v% "rule_law" <- df_attr$`Rule of Law`
net_countries %v% "voice_acc" <- df_attr$`Voice and Accountability`
net_countries %v% "gini" <- df_attr$gini_mean

#### ergms ####
f0 <- ergm(net_countries ~ edges) # null model
# Governance models
f1 <- ergm(net_countries ~ edges + absdiff("corruption") + absdiff("gov_effectiveness") +
               absdiff("pol_stability") + absdiff("regulatory_qual") + 
               absdiff("rule_law") + absdiff("voice_acc"))
f2 <- ergm(net_countries ~ edges + absdiff("corruption") +
               absdiff("pol_stability") + absdiff("voice_acc"))
# governance and inequality models
f3 <- ergm(net_countries ~ edges + absdiff("corruption") + absdiff("gov_effectiveness") +
               absdiff("pol_stability") + absdiff("regulatory_qual") + 
               absdiff("rule_law") + absdiff("voice_acc") + absdiff("gini"))
f4 <- ergm(net_countries ~ edges + absdiff("corruption") + absdiff("gov_effectiveness") +
               absdiff("pol_stability") + absdiff("regulatory_qual") + 
               absdiff("rule_law") + absdiff("voice_acc") + absdiff("gini") +
               nodeicov("corruption") + nodeicov("rule_law") + nodeocov("corruption") +
               nodeocov("rule_law") )
# structural models
f5 <- ergm(net_countries ~ edges + triadcensus())


summary(f4)
tidy(f4, exponentiate = TRUE)
gof(f4)

## add network attributes (this is for visualization)
df_attr |> 
    select(-nodes) |> 
    GGally::ggpairs()


net_countries %v% "indegree" <- sna::degree(
    net_countries, gmode = "digraph", diag = TRUE, rescale = FALSE, 
    cmode = "indegree")

net_countries %v% "outdegree" <- sna::degree(
    net_countries, gmode = "digraph", diag = TRUE, rescale = FALSE, 
    cmode = "outdegree")

net_countries %v% "degree" <- sna::degree(
    net_countries, gmode = "digraph", diag = TRUE, rescale = FALSE)
