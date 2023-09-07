# Extract key investors per case study
library(tidyverse)
library(sna)
library(ggnetwork)
library(sf)
library(spData)
library(scico)
library(ggnewscale)
library(ggrepel)
library(patchwork)

#### load data ####
load("data/investors_cleaned.RData")
load("data/casestudies.RData")
load("data/shr_class.RData")

dat ## companies - shareholders dataset
df1 # company - commodities dataset
df0 # case studies classification

dat <- dat |>
    ## Filtering ownership > 0.01 %: reduces obs from 4609 to 3621
    #filter(ownership > 0.01) |> 
    mutate(holdings = ownership * 0.05)

#### Cleaning steps ####
# Exclude nodes that are not countries:
no_country <- c("Central Europe", "Eastern Europe", "Middle East", "Southern Europe", "Europe", "Fejér", "Taiwan")


case_df <-  df1 %>% unique() |> 
    select(company, commodity, casestudy, country, origin, type) %>% 
    #group_by(origin, country, casestudy) %>% 
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
    filter(!is.na(origin), !is.na(country)) %>% 
    # Get rid of destinations that are not countries
    filter(!(origin %in% no_country), !(country %in% no_country)) |> 
    # remove "Liechtenstein" to include inequality metrics:
    filter(origin != "Liechtenstein" ) |> 
    ungroup() |>
    filter(casestudy != "Other", type != "Private") |> 
    # unique is needed because some companies operate in different subregions 
    # within a country
    unique() |> 
    select(-country, -origin) |> 
    ## correct names for merging with investors data:
    mutate(
        company = case_when(
            company == "Golden Energy And Resources Ltd" ~ "Golden Energy And Resources Limited" ,
            company == "Pt Astra Agro Lestari Tbk" ~ "Pt Astra Agro Lestari Tbkê",
            company == "Nine Dragons Paper Holdings" ~ "Nine Dragons Paper (Holdings) Limited",
            TRUE ~ company
        )
    ) |> unique()

case_df

case_df$company [!case_df$company %in% dat$company] |> unique() # 4 in case_df but not in dat
dat$company[!dat$company %in% case_df$company] |> unique() # 45 in dat but not in case_df

unique(dat$company)[!unique(dat$company) %in% unique(df1$company)]

case_df |> pull(company) |> unique()  # 57 companies
dat |> pull(company) |> unique() # 99 companies

case_df <- case_df |> 
    left_join(
        dat |> rename(shr_country = country, shr_type = type)
    ) ## 6.3k rows with countries; 3.9k without

case_df |> filter(is.na(guo_final)) |>  pull(company) |> unique() 

#remove the problematic cases for now
case_df <- case_df |> 
    filter(!is.na(guo_final)) # 
#filter(direct_percent > 0.01) # 0.01 is Neglegible (NG) in Orbis language

unique(case_df$shareholder) %in% shr_class$shareholder |> sum()


case_df <- case_df |> 
    left_join(shr_class) |> 
    #group_by(shareholder, casestudy, shr_type) |> 
    mutate(shr_type2 = str_sub(class, start = 1L, end = 1L)) |> 
    mutate(shr_type2 = case_when(
        # this is the NACE classification
        #is.na(shr_type2) ~ "Missing",
        shr_type2 == "A" ~ "Agriculture, forestry and fishing",
        shr_type2 == "B" ~ "Mining and quarrying",
        shr_type2 == "C" ~ "Manufacturing",
        shr_type2 == "D" ~ "Electricity, gas, steam and air conditioning supply",
        shr_type2 == "F" ~ "Construction",
        shr_type2 == "G" ~ "Wholesale and retail trade",
        shr_type2 == "H" ~ "Transportation and storage",
        shr_type2 == "I" ~ "Accommodation and food service activities",
        shr_type2 == "J" ~ "Information and communication",
        shr_type2 == "K" ~ "Financial and insurance activities",
        shr_type2 == "L" ~ "Real estate activities",
        shr_type2 == "M" ~ "Professional, scientific and technical activities",
        shr_type2 == "N" ~ "Administrative and support service activities",
        shr_type2 == "O" ~ "Public administration and defence",
        shr_type2 == "P" ~ "Education",
        shr_type2 == "Q" ~ "Human health and social work activities",
        shr_type2 == "R" ~ "Arts, entertainment and recreation",
        shr_type2 == "S" ~ "Other service activities"
        #shr_type2 == "" ~ "I"
    )) |> # below the orginal Orbis classification
    mutate(shr_type = case_when(
        is.na(shr_type) ~ "Missing",
        shr_type == "A" ~ "Insurance company",
        shr_type == "B" ~ "Bank",
        shr_type == "C" ~ "Corporate companies",
        shr_type == "D" ~ "Unnamed private shareholders",
        shr_type == "F" ~ "Financial company",
        #shr_type == "G" ~ "Wholesale and retail trade",
        shr_type == "H" ~ "Self ownership",
        shr_type == "I" ~ "One or more known individuals or families",
        shr_type == "J" ~ "Foundation or Research Institute",
        shr_type == "Z" ~ "Public",
        shr_type == "L" ~ "Employees/Managers/Directors",
        shr_type == "M" ~ "Professional, scientific and technical activities",
        shr_type == "V" ~ "Venture capital",
        shr_type == "Y" ~ "Hedge fund",
        shr_type == "P" ~ "Private equity firms",
        shr_type == "Q" ~ "Branch",
        shr_type == "W" ~ "Marine vessel",
        shr_type == "S" ~ "Public authorities, States, Governments"
        #shr_type == "" ~ "I"
    )) 

### size ownership
publicomp <- read_csv2(
    file = "data/Paulas_files/002-PublicCompanies.csv", 
    locale = locale(encoding = "latin1")) |> 
    janitor::clean_names()

publicomp <- publicomp |> 
    #filter(type == "Public") |>
    mutate(company = str_to_title(company)) |> 
    select(company, market_cap_mll) |> unique()

#### network ####
# 220415: Forcing the network to be bipartite, only two of the >1800 actors show in both classes:
# - Bnp Paribas
# - Oji Holdings Corporation
### Bipartite
bip_net <- case_df |> ungroup() |> 
    mutate(company = case_when(
        #company == "Bnp Paribas" ~ "Bnp Paribas_c",
        company == "Oji Holdings Corporation" ~ "Oji Holdings Corporation_c",
        TRUE ~ company
    )) |> #pull(company) |> unique() |> length()
    select(shareholder, company, ownership, holdings) |> unique() |> 
    filter(!is.na(shareholder)) |> 
    filter(!shareholder %in% c("Public", "Self Owned")) |> 
    network(directed = FALSE, bipartite = TRUE, matrix.type = "edgelist", 
            ignore.eval=FALSE, multiple = FALSE)

bib_mat <- bip_net |> as.sociomatrix.sna()
dim(bib_mat) # 1819 shareholders, 54 companies

## bipartite network with all 99 companies regardless of case study

net2 <- dat |> 
    select(shareholder, company, ownership, holdings, op_revenue) |> 
    unique() |> 
    # check missing values
    # filter(is.na(op_revenue)) # 1858 / 4540 links
    filter(!shareholder %in% c("Public", "Self Owned")) |> 
    mutate(pow = (ownership/100)*op_revenue) |> 
    igraph::graph_from_data_frame(directed = TRUE)
    # network(directed = TRUE, bipartite = FALSE, matrix.type = "edgelist", 
    #         ignore.eval=FALSE, multiple = FALSE)

net2

#### Revisions ####

# reviewer wants eigen based metrics of centrality, some of them easier to calculate in weighted form in igraph
library(igraph)

net <- igraph::graph_from_data_frame(
    d = case_df |> dplyr::select(shareholder, company, ownership, op_revenue),
    directed = TRUE
    )


df_stats <- tibble(
    vertex = get.vertex.attribute(net, "name"),
    eigen = eigen_centrality(net, directed = TRUE, scale = TRUE, weights = E(net)$ownership)$vector,
    power = power_centrality(net, rescale = TRUE),
    alpha = alpha_centrality(net, weights = E(net)$ownership) # for directed graphs
)

df_stats <-  df_stats |> 
    mutate(class = ifelse(
        vertex %in% (case_df |> pull(company) |> unique()), # this line for case_df
       #vertex %in% unique(dat$company),
       "company",
        "shareholder"
    )) 

df_stats |> 
    ggplot(aes(power, alpha)) +
    geom_point(aes(color = class), size = 0.5, alpha= 0.5)

df_stats |> 
    select(-eigen) |> 
    pivot_longer(cols = power:alpha, names_to = "variable", values_to = "values") |> 
    group_by(variable) |> 
    top_n(n = 25) |> 
    ggplot(aes(values, vertex)) +
    geom_col(aes(fill = class), position = "stack") +
    tidytext::scale_y_reordered() +
    facet_wrap(~variable, scales = "free") +
    theme_light(base_size = 6)

a <- df_stats |> 
    mutate(class = as_factor(class)) |> 
    top_n(25, wt = power) |> 
    arrange(power) |> 
    mutate(vertex = as_factor(vertex)) |> 
    ggplot(aes(power, vertex)) +
    geom_col(aes(fill = class)) +
    scale_fill_manual(values = c("goldenrod")) + 
    labs(tag = "A", y = "Actors", x = "Power centrality") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.7, 0.1), legend.key.size = unit(2,"mm"),
          axis.text.x = element_text(size = 4))

b <- df_stats |> 
    mutate(class = as_factor(class)) |> 
    top_n(25, wt = alpha) |> 
    arrange(alpha) |> 
    mutate(vertex = as_factor(vertex)) |> 
    ggplot(aes(alpha, vertex)) +
    geom_col(aes(fill = class)) +
    scale_fill_manual(values = c("lightblue")) +
    labs(tag = "B", y = "Actors", x = "Alpha centrality") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.7, 0.1), legend.key.size = unit(2,"mm"),
          axis.text.x = element_text(size = 4))

c <- case_df |>  #dat |> #
    select(company, shareholder, ownership, op_revenue, guo_final) |> 
    unique() |> 
    group_by(shareholder) |>
    mutate( p = (ownership / 100) * op_revenue) |> 
    # ungroup() |> group_by(guo_final) |> 
    summarize(power = sum(p, na.rm = TRUE)) |> 
    arrange(desc(power)) |> 
    top_n(25) |> 
    mutate(shareholder = as_factor(shareholder) |> fct_rev()) |> 
    ggplot(aes(power, shareholder)) +
    geom_col() +
    labs(x = "Portfolio value\nmillions US$", y = "Shareholders", tag = "C") +
    theme_light(base_size = 6) + theme(axis.text.x = element_text(size = 4))

d <- case_df |> #dat |> #
    select(company, shareholder, ownership, op_revenue, guo_final) |> 
    unique() |> 
    group_by(shareholder) |>
    mutate( p = (ownership / 100) * op_revenue) |> 
    ungroup() |> group_by(guo_final) |> 
    summarize(power = sum(p, na.rm = TRUE)) |> 
    arrange(desc(power)) |> 
    top_n(25) |> 
    mutate(guo_final = as_factor(guo_final) |> fct_rev()) |> 
    ggplot(aes(power, guo_final)) +
    geom_col() +
    labs(x = "Portfolio value\nmillions US$", y = "Global Ultimate Owners", tag = "D") +
    theme_light(base_size = 6) + theme(axis.text.x = element_text(size = 4))


ggsave(
    plot = (a+b)/(c+d), 
    path = "figures/", file = "fig5_key_actors_54comps.png",
    dpi = 500, bg = "white", width = 7.5, height = 6
)

library(naniar)

case_df |> 
    select(company, shareholder, ownership, op_revenue, guo_final, 
           shr_type2) |> 
    unique() |> 
    group_by(shareholder) |> 
    mutate( p = (ownership / 100) * op_revenue) |>
    ggplot(aes(ownership, p)) +
    geom_miss_point(alpha = 0.5) + 
    geom_vline(xintercept = 20, color = "orange", linetype = 2) +
    labs(y = "Portfolio value\n million US$", x = "Ownership in %") +
    theme_light(base_size = 6)

ggsave(
    file = "figures/fig_reviewer.png", device = "png", plot = last_plot(),
    width = 3, height = 2.5, dpi = 500, bg = "white"
)

df_summary <- case_df |> 
    select(company, shareholder, ownership, op_revenue, guo_final, 
           shr_type2) |>
    unique() |> 
    group_by(shareholder) |> 
    summarize(
        mean_own = mean(ownership, na.rm = TRUE),
        mean_rev = mean(op_revenue, na.rm = TRUE),
        min_own = min(ownership, na.rm = TRUE),
        max_own = max(ownership, na.rm = TRUE),
        min_rev = min(op_revenue, na.rm = TRUE),
        max_rev = max(op_revenue, na.rm = TRUE))

df_summary |> 
    ggplot(aes(mean_own, mean_rev)) +
    geom_miss_point(size = 0.5, alpha = 0.5) +
    geom_errorbarh(
        aes(xmin = min_own, xmax = max_own), 
        color = "grey50", linewidth = 0.25) +
    # geom_errorbar(
    #     aes(ymin = min_rev, ymax = max_rev),
    #     data = df_summary |> 
    #         filter(min_rev != -Inf, max_rev != Inf) |> 
    #         filter(!is.na(mean_rev)),
    #     color = "gold", linewidth = 2) +
    labs(x = "Mean ownership", y = "Mean revenue") +
    theme_light(base_size = 6)

ggsave(
    file = "figures/fig_reviewer2.png", device = "png", plot = last_plot(),
    width = 3, height = 2.5, dpi = 500, bg = "white"
)


# weighted bipartite matrix
sm_fig <- case_df |> 
    mutate(
        company = fct_reorder(company, ownership, mean, na.rm = TRUE),
        shareholder = fct_reorder(shareholder, ownership, mean, na.rm = TRUE)
    ) |> #pull(company) |> unique() |> length()
    ggplot(aes(shareholder, company)) +
    geom_tile(aes(fill = ownership)) +
    scale_fill_viridis_c(
        na.value = "grey50",
        guide = guide_colorbar(
            barwidth = unit(2, 'mm'), barheight = unit(3, 'cm')
        )) +
    theme_light(base_size = 6) +
    theme(axis.text.x = element_blank())

# ggsave(
#     sm_fig, path = "figures/", file = "sm_ownership_matrix.png",
#     dpi = 500, width = 6, height = 5, bg = "white"
# )

#### Revising raw data ####
## this is how the data was read and imported:
fls <- dir_ls("data/Paulas_files/Financial_actors/")
dat <- map(
    fls,
    function(x) {
        x |> 
            read_delim(
                delim = ";", col_types = "cccccccccc", name_repair = "minimal",
                col_names = c("company", "name", "pas", "type", "direct_percent", "total_percent", "source", "date", "op_revenue", "employees"),
                skip = 1, na = c("n.d.", "-")) |> 
            janitor::clean_names() }
) 

dat <- map2(
    .x = dat, .y=fls, .f = function(x,y) {
        x$file_name <- y; 
        return(x)}
)

dat <- dat |> 
    bind_rows() 

# dat |> 
#     mutate(file_name = str_remove(file_name, "data/Paulas_files/Financial_actors/")) |> 
#     select(file_name, company) |> 
#     unique()

dat <- dat |> 
    filter(company != "") |> # 2 empty rows
    mutate(
        company = str_to_title(company),
        name = str_to_title(name),
        total_percent = str_remove_all(total_percent, pattern = "±")
    ) |> 
    mutate(
        total_percent = as.numeric(total_percent),
        direct_percent = as.numeric(direct_percent), 
        date = lubridate::my(date),
        op_revenue = as.numeric(op_revenue),
        employees = as.numeric(employees)
    )



skimr::skim(dat)

# save(dat, file = "data/investors_cleaned.RData")

df1 <- readxl::read_xlsx(
    path = "data/Paulas_files/001-Drivers_Increasing_Risk_ED.xlsx",
    sheet = 2,
    na = "NA") %>% 
    janitor::clean_names()