## exponential random graph models of financial actors
library(tidyverse)
library(sna)
library(ergm)
library(broom)
library(ggnetwork)
library(patchwork)

#### Load data ####

#alternatively: it contains the case study column needed for spliting.
# load data
load("data/investors_cleaned.RData")
load("data/casestudies.RData")

#### Network of countries per case study ####
# This first analysis merge all commodities into "geographies". Subsequent analysis 
# could separate by commodity and see if it holds
# Exclude nodes that are not countries:
no_country <- c("Central Europe", "Eastern Europe", "Middle East", "Southern Europe", "Europe", "Fejér", "Taiwan")
case_list <-  df1 %>% 
    select(company, origin, country, casestudy) %>% 
    group_by(origin, country, casestudy) %>% 
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
    # unique() %>% 
    count(name = "channel") %>% # a channel is a country * commodities
    filter(!is.na(origin), !is.na(country)) %>% 
    # Get rid of destinations that are not countries
    filter(!(origin %in% no_country), !(country %in% no_country)) |> 
    # remove "Liechtenstein" to include inequality metrics:
    filter(origin != "Liechtenstein" ) |> 
      ungroup() |>
    select(origin,country, channel, casestudy) |> 
    split(~casestudy)
    
net_list <-  case_list[-10] |> 
    map(function(x) network(x,
        directed = TRUE, bipartite = FALSE, matrix.type = "edgelist", 
        ignore.eval=FALSE, loops = TRUE))


node_list <- net_list |> 
    map(function(x) network.vertex.names(x))

nodes <- unlist(node_list) |> unique()
nodes_df <- tibble(
    cs = rep(names(node_list), unlist(map(node_list, length))),
    node = unlist(node_list)
)

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

# the previous approach don't work with multiple networks, so wrap it into a function
net_list <- net_list |> 
    map(function (x) {
        n <- network.vertex.names(x)
        d <- df_attr |> filter(nodes %in% n)
        x %v% "corruption" <- d$`Control of Corruption`
        x %v% "gov_effectiveness" <- d$`Government Effectiveness`
        x %v% "pol_stability" <- d$`Political Stability and Absence of Violence/Terrorism`
        x %v% "regulatory_qual" <- d$`Regulatory Quality`
        x %v% "rule_law" <- d$`Rule of Law`
        x %v% "voice_acc" <- d$`Voice and Accountability`
        x %v% "gini" <- d$gini_mean
        
        return(x)
    })


#### ergms ####
f0 <- map(net_list, function(x) ergm(x ~ edges)) # null model
# Governance models
f1 <- map(net_list, function (x) ergm(
    x ~ edges + absdiff("corruption") + absdiff("gov_effectiveness") +
               absdiff("pol_stability") + absdiff("regulatory_qual") + 
               absdiff("rule_law") + absdiff("voice_acc")))

f2 <-  map(net_list, function (x) ergm(
    x ~ edges + absdiff("corruption") +
               absdiff("pol_stability") + absdiff("voice_acc")))

# governance and inequality models
f3 <-  map(net_list, function (x) ergm(
    x ~ edges + absdiff("corruption") + absdiff("gov_effectiveness") +
               absdiff("pol_stability") + absdiff("regulatory_qual") + 
               absdiff("rule_law") + absdiff("voice_acc") + absdiff("gini")))

f4 <-  map(net_list, function (x) ergm(
    x ~ edges + absdiff("corruption") + absdiff("gov_effectiveness") +
               absdiff("pol_stability") + absdiff("regulatory_qual") + 
               absdiff("rule_law") + absdiff("voice_acc") + absdiff("gini") +
               nodeicov("corruption") + nodeicov("gov_effectiveness") + nodeicov("pol_stability") +
               nodeicov("regulatory_qual") + nodeicov("rule_law") + nodeocov("corruption") +
               nodeocov("gov_effectiveness") + nodeocov("pol_stability") +
               nodeocov("regulatory_qual") + nodeocov("rule_law") + nodeicov("gini") + nodeocov("gini")))


fits <- c(f0, f1, f2, f3, f4)
fit_name <- c(rep("null",9), rep("full_gov",9), rep("short_gov",9), rep("short_gov+ineq",9),
              rep("full_gov+ineq",9))

df_stats <- tibble(
    model = fit_name,
    case = names(fits),
    logLik = map_dbl(fits, logLik),
    AIC = map_dbl(fits, AIC),
    x = rep(2, 45),
    y = rep(1, 45)
)


fits <- fits |> map(tidy, exponentiate = FALSE) 
fits <- map2(fits, fit_name, function(x,y) {x$model <- y; return(x)})
fits <- map2(fits, names(fits), function(x,y) {x$case <- y; return(x)})

fits |> 
    bind_rows() |> 
    mutate(p_value = case_when(
        p.value < 0.05 ~ "p < 0.05",
        p.value > 0.05 & p.value <= 0.1 ~ "p < 0.1",
        p.value > 0.1 ~ "p > 0.1")) |> 
    mutate(model = as_factor(model)) |> 
    ggplot(aes(estimate, term)) + 
    geom_vline(aes(xintercept = 0), color = "gray50", linetype = 2) +
    geom_point(aes(color = p_value)) +
    geom_errorbarh(
        aes(xmin = estimate - std.error, xmax = estimate + std.error, color = p_value)) + 
    #geom_text(aes( label = logLik), data = df_stats) +
    facet_wrap(model ~ case, scales = 'free') +
    theme_linedraw()

ggsave(
    filename = "country_netmodels.png",
    plot = last_plot(),
    device = "png",
    path = "figures/",
    width = 8, height = 3,
    bg = "white", dpi = 400
)


df_stats |> 
    ggplot(aes(logLik, AIC)) +
    geom_point(aes(color = case, shape = model))

a <- df_stats |> 
    ggplot(aes(AIC, model)) +
    geom_col() +
    facet_wrap(~case) +
    labs(tag = "A") +
    theme_linedraw(base_size = 6)
b <- df_stats |> 
    ggplot(aes(logLik, model)) +
    geom_col() +
    facet_wrap(~case) +
    labs(tag = "B") +
    theme_linedraw(base_size = 6)

a+b

ggsave("model_comparison.png", plot = a+b, device = "png", path = "figures/",
       width = 6, height = 3)

nets <- 
    map2(net_list, names(case_list[-10]), function(w,z){
        ggplot(ggnetwork(w),
               aes(x = x, y = y, xend = xend, yend = yend)) +
            geom_edges(
                aes(color = channel), size = 0.5, alpha = 1,
                arrow = arrow(length = unit(2, "pt"), type = "closed")) +
            #geom_nodes(aes(size = outdegree), color = "red", alpha = 0.4) +
            geom_nodes(color = "blue", alpha = 0.4, size = 2) +
            geom_nodetext(aes(label = vertex.names), size = 1.5) +
            scico::scale_color_scico(
                "Companies", palette = "batlow", direction = -1,
                guide = guide_colorbar(
                    barwidth = unit(2, "mm"), barheight = unit(1, "cm"))) +
            labs(title = z) +
            theme_blank(base_size = 6) 
    }
)
    
wrap_plots(nets) + plot_layout(nrow =3)

ggsave(
    filename = "networks_countries.png",
    plot = wrap_plots(nets) + plot_layout(nrow =3),
    device = "png",
    path = "figures/",
    width = 5, height = 5,
    dpi = 400, bg = "white"
)

#### Network countries sharing companies: One single network ####
net_countries <-  df1 %>% 
    select(company, origin, country) %>% 
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
    group_by(origin, country) %>% 
    # unique() %>% 
    count(name = "channel") %>% # a channel is a country * commodities
    filter(!is.na(origin), !is.na(country)) %>% 
    # Get rid of destinations that are not countries
    filter(!(origin %in% no_country), !(country %in% no_country)) |> 
    # remove "Liechtenstein" to include inequality metrics:
    filter(origin != "Liechtenstein" ) |> 
    ungroup() |> #unique() |> 
    network(directed = TRUE, bipartite = FALSE, matrix.type = "edgelist", 
            ignore.eval=FALSE, loops = TRUE)

nodes <- network.vertex.names(net_countries)
nodes # note they are not alphabetical

## Reimport inequality and wgi data, they were trimmed in the previous version of the analysis:
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
f1 <- ergm(
    net_countries ~ edges + absdiff("corruption") + absdiff("gov_effectiveness") +
        absdiff("pol_stability") + absdiff("regulatory_qual") + 
        absdiff("rule_law") + absdiff("voice_acc"))

f2 <-  ergm(
    net_countries ~ edges + absdiff("corruption") +
        absdiff("pol_stability") + absdiff("voice_acc"))

# governance and inequality models
f3 <-  ergm(
    net_countries ~ edges + absdiff("corruption") + absdiff("gov_effectiveness") +
        absdiff("pol_stability") + absdiff("regulatory_qual") + 
        absdiff("rule_law") + absdiff("voice_acc") + absdiff("gini"))

f4 <-  ergm(
    net_countries ~ edges + absdiff("corruption") + absdiff("gov_effectiveness") +
        absdiff("pol_stability") + absdiff("regulatory_qual") + 
        absdiff("rule_law") + absdiff("voice_acc") + absdiff("gini") +
        nodeicov("corruption") + nodeicov("gov_effectiveness") + nodeicov("pol_stability") +
        nodeicov("regulatory_qual") + nodeicov("rule_law") + nodeocov("corruption") +
        nodeocov("gov_effectiveness") + nodeocov("pol_stability") +
        nodeocov("regulatory_qual") + nodeocov("rule_law") + nodeicov("gini") + nodeocov("gini"))


fits <- list(f0, f1, f2, f3, f4)
fit_name <- c("null", "governance","short_gov","gov+ineq","gov+ineq+covars")

df_stats <- tibble(
    model = fit_name,
    case = names(fits),
    logLik = map_dbl(fits, logLik),
    AIC = map_dbl(fits, AIC)
)

#### structural models ####
#f5 <- ergm(net_countries ~ edges + triadcensus())


summary(f4)
tidy(f4, exponentiate = TRUE)
gof(f4)


fits <- fits |> map(tidy, exponentiate = FALSE) 
fits <- map2(fits, fit_name, function(x,y) {x$model <- y; return(x)})
#fits <- map2(fits, names(fits), function(x,y) {x$case <- y; return(x)})

a <- fits |> 
    bind_rows() |> 
    mutate(p_value = case_when(
        p.value < 0.05 ~ "p < 0.05",
        p.value > 0.05 & p.value <= 0.1 ~ "p < 0.1",
        p.value > 0.1 ~ "p > 0.1")) |> 
    mutate(model = as_factor(model)) |> 
    mutate(term = fct_relevel(term, "edges")) |>
    mutate(term = fct_rev(term)) |> 
    ggplot(aes(estimate, term)) + 
    geom_vline(aes(xintercept = 0), color = "gray50", linetype = 2) +
    geom_point(aes(color = p_value), size = 1) +
    geom_errorbarh(
        aes(xmin = estimate - std.error, xmax = estimate + std.error, color = p_value),
        height = 0.5, size = 0.25) + 
    #geom_text(aes( label = logLik), data = df_stats) +
    facet_wrap(~model, ncol = 5) +
    labs(tag = "A") +
    theme_linedraw(base_size = 6) +
    theme(legend.position = c(0.06,0.2))

b <- ggplot(df_stats, aes(logLik, model)) +
    geom_col() +
    labs(tag = "B") +
    theme_light(base_size = 7)
c <- ggplot(df_stats, aes(AIC, model)) +
    geom_col() +
    labs(tag = "C") +
    theme_light(base_size = 7)

ggsave(
    filename = "country_netmodels_220126.png",
    plot = a/( b+c + d) ,
    device = "png",
    path = "figures/",
    width = 7, height = 5,
    bg = "white", dpi = 400
)


## add network attributes (this is for visualization)
net_countries %v% "indegree" <- sna::degree(
    net_countries, gmode = "digraph", diag = TRUE, rescale = FALSE, 
    cmode = "indegree")

net_countries %v% "outdegree" <- sna::degree(
    net_countries, gmode = "digraph", diag = TRUE, rescale = FALSE, 
    cmode = "outdegree")

net_countries %v% "degree" <- sna::degree(
    net_countries, gmode = "digraph", diag = TRUE, rescale = FALSE)


df_attr <- df_attr |> 
    add_column(indegree = net_countries %v% "indegree",
               outdegree = net_countries %v% "outdegree", 
               degree = net_countries %v% "degree")

df_attr |> 
    select(-nodes, -isoa2, -country_code) |> 
    GGally::ggpairs() +
    theme_light(base_size = 10)

## correlations
ggsave(
    filename = "correlations.png",
    plot = last_plot(),
    device = "png",
    path = "figures/",
    width = 7, height = 7,
    bg = "white", dpi = 400
)

d <- df_attr |> 
    ggplot(aes(indegree, outdegree)) +
    geom_point(aes(color = `Government Effectiveness`), size = 3) +
    geom_text(aes(label = isoa2), color = "white", size = 2) + 
    geom_abline(slope = 1, intercept = 0, color = "red") +
    scico::scale_color_scico(
        palette = "berlin",
        guide = guide_colourbar(title.position = "top")) +
    labs(tag = "D") +
    theme_light(base_size = 5) +
    theme(legend.position = "bottom", legend.key.height = unit(0.15, "cm"),
          legend.key.width = unit(0.5, "cm"))

ggsave(
    filename = "net_stats.png",
    plot = last_plot(),
    device = "png",
    path = "figures/",
    width = 4, height = 3,
    bg = "white", dpi = 400
)              

### Network of countries given common shareholders
## I need to recompile again the list of case studies with country of destination in it. It is a slighthly larged dataset that the used previously, because companies can appear multiple times within a country if they expoit different subnational regions. 

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
    select( -origin) |> # keeping -country, so I can link country of shareholder and country of company operations
    ## correct names for merging with investors data:
    mutate(
        company = case_when(
            company == "Golden Energy And Resources Ltd" ~ "Golden Energy And Resources Limited" ,
            company == "Pt Astra Agro Lestari Tbk" ~ "Pt Astra Agro Lestari Tbkê",
            company == "Nine Dragons Paper Holdings" ~ "Nine Dragons Paper (Holdings) Limited",
            TRUE ~ company
        )
    ) 

case_df

case_df$company [!case_df$company %in% dat$company] |> unique() # 4 in case_df but not in dat
dat$company[!dat$company %in% case_df$company] |> unique() # 45 in dat but not in case_df

case_df |> pull(company) |> unique()  # 54 companies
dat |> pull(company) |> unique() # 99 companies

case_df <- case_df |> 
    left_join(dat |> rename(shr_country = country, shr_type = type)) ## 6.3k rows with countries; 3.9k without

case_df |> filter(is.na(guo_final)) |>  pull(company) |> unique() 

#remove the problematic cases for now
case_df <- case_df |> 
    filter(!is.na(guo_final)) # 
#filter(direct_percent > 0.01) # 0.01 is Neglegible (NG) in Orbis language

### Now with case_df I can build bipartite directed networks
shr_net <- case_df |> 
    group_by(shr_country, country) |> 
    summarize(n = n()) |> 
    left_join(df_attr |> select(country = nodes, comp_country = isoa2)) |> 
    select(shr_country, comp_country, n) |> 
    filter(!is.na(shr_country)) |> 
    network( directed = TRUE, matrix.type = "edgelist", 
            ignore.eval=FALSE, loops = TRUE)

df_attr2 <- tibble(
    nodes = network.vertex.names(shr_net),
    indegree = sna::degree(shr_net, gmode = "digraph", cmode = "indegree"), 
    outdegree = sna::degree(shr_net, gmode = "digraph", cmode = "outdegree"), 
    degree = sna::degree(shr_net, gmode = "digraph", cmode = "freeman" ), 
    bet = sna::betweenness(shr_net, gmode = "digraph")) |>
    left_join(inq, by = c("nodes" = "isoa2")) |> 
    left_join(df_wgi, by = c("country" = "country_name") )

df_attr2 |> print(n=55) # there are missing values: impute them!
## imputed countries are: 
df_attr2 |> filter(
    is.na(gini_mean) | is.na(`Control of Corruption`) | is.na(`Government Effectiveness`) |
    is.na(`Political Stability and Absence of Violence/Terrorism`) | is.na(`Regulatory Quality`) |
    is.na(`Rule of Law`) | is.na(`Voice and Accountability`))
# United Arab Emirates, The Bahamas, Kuwait, Qatar, Taiwan, Bermuda, Virgin Islands (UK) 

df_attr2 <- df_attr2 |> 
    naniar::impute_mean_if(.predicate = is.numeric )

ggplot(df_attr2, aes(indegree, outdegree)) +
    geom_point(aes(size = bet, color = `Government Effectiveness`), alpha = 0.75) +
    geom_text(aes(label = nodes)) + 
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = 2) +
    scale_color_gradient2()

## add network attributes
shr_net %v% "corruption" <- df_attr2$`Control of Corruption`
shr_net %v% "gov_effectiveness" <- df_attr2$`Government Effectiveness`
shr_net %v% "pol_stability" <- df_attr2$`Political Stability and Absence of Violence/Terrorism`
shr_net %v% "regulatory_qual" <- df_attr2$`Regulatory Quality`
shr_net %v% "rule_law" <- df_attr2$`Rule of Law`
shr_net %v% "voice_acc" <- df_attr2$`Voice and Accountability`
shr_net %v% "gini" <- df_attr2$gini_mean

## ergms
#### ergms ####
s0 <- ergm(shr_net ~ edges) # null model
# Governance models
s1 <- ergm(
    shr_net ~ edges + absdiff("corruption") + absdiff("gov_effectiveness") +
        absdiff("pol_stability") + absdiff("regulatory_qual") + 
        absdiff("rule_law") + absdiff("voice_acc"))

s2 <-  ergm(
    shr_net ~ edges + absdiff("corruption") +
        absdiff("pol_stability") + absdiff("voice_acc"))

# governance and inequality models
s3 <-  ergm(
    shr_net ~ edges + absdiff("corruption") + absdiff("gov_effectiveness") +
        absdiff("pol_stability") + absdiff("regulatory_qual") + 
        absdiff("rule_law") + absdiff("voice_acc") + absdiff("gini"))

s4 <-  ergm(
    shr_net ~ edges + absdiff("corruption") + absdiff("gov_effectiveness") +
        absdiff("pol_stability") + absdiff("regulatory_qual") + 
        absdiff("rule_law") + absdiff("voice_acc") + absdiff("gini") +
        nodeicov("corruption") + nodeicov("gov_effectiveness") + nodeicov("pol_stability") +
        nodeicov("regulatory_qual") + nodeicov("rule_law") + nodeocov("corruption") +
        nodeocov("gov_effectiveness") + nodeocov("pol_stability") +
        nodeocov("regulatory_qual") + nodeocov("rule_law") + nodeicov("gini") + nodeocov("gini"))


fits2 <- list(s0, s1, s2, s3, s4)
fit_name <- c("null", "governance","short_gov","gov+ineq","gov+ineq+covars")

df_stats2 <- tibble(
    model = fit_name,
    case = names(fits2),
    logLik = map_dbl(fits2, logLik),
    AIC = map_dbl(fits2, AIC)
)

### Produce the same figures but for stakeholder networks

fits2 <- fits2 |> map(tidy, exponentiate = FALSE) 
fits2 <- map2(fits2, fit_name, function(x,y) {x$model <- y; return(x)})
#fits <- map2(fits, names(fits), function(x,y) {x$case <- y; return(x)})

a <- fits2 |> 
    bind_rows() |> 
    mutate(p_value = case_when(
        p.value < 0.05 ~ "p < 0.05",
        p.value > 0.05 & p.value <= 0.1 ~ "p < 0.1",
        p.value > 0.1 ~ "p > 0.1")) |> 
    mutate(model = as_factor(model)) |> 
    mutate(term = fct_relevel(term, "edges")) |>
    mutate(term = fct_rev(term)) |> 
    ggplot(aes(estimate, term)) + 
    geom_vline(aes(xintercept = 0), color = "gray50", linetype = 2) +
    geom_point(aes(color = p_value), size = 1) +
    geom_errorbarh(
        aes(xmin = estimate - std.error, xmax = estimate + std.error, color = p_value),
        height = 0.5, size = 0.25) + 
    #geom_text(aes( label = logLik), data = df_stats) +
    facet_wrap(~model, ncol = 5) +
    labs(tag = "A") +
    theme_linedraw(base_size = 6) +
    theme(legend.position = c(0.06,0.2))

b <- ggplot(df_stats2, aes(logLik, model)) +
    geom_col() +
    labs(tag = "B") +
    theme_light(base_size = 7)
c <- ggplot(df_stats2, aes(AIC, model)) +
    geom_col() +
    labs(tag = "C") +
    theme_light(base_size = 7)

d <- df_attr2 |> 
    ggplot(aes(indegree, outdegree)) +
    geom_point(aes(color = `Government Effectiveness`), size = 3) +
    geom_text(aes(label = nodes), color = "white", size = 2) + 
    geom_abline(slope = 1, intercept = 0, color = "red") +
    scico::scale_color_scico(
        palette = "berlin",
        guide = guide_colourbar(title.position = "top")) +
    labs(tag = "D") +
    theme_light(base_size = 5) +
    theme(legend.position = "bottom", legend.key.height = unit(0.15, "cm"),
          legend.key.width = unit(0.5, "cm"))

ggsave(
    filename = "country_shareholder_netmodels_220505.png",
    plot = a/( b+c + d) ,
    device = "png",
    path = "figures/",
    width = 7, height = 5,
    bg = "white", dpi = 400
)

## maps
library(sf)
library(spData)
data(world)
df_cntr <- world |> 
    #filter(!is.na(pop)) |> # remove non-countries aggregations
    st_centroid(of_largest = TRUE) |> 
    select(iso_a2, name_long, geom)

world <- ggplot(map_data("world"), aes(x = long, y = lat)) +
    geom_polygon(aes(group = group), color = "grey65",
                 fill = "#f9f9f9", size = 0.1) +
    #coord_map(projection = "mercator" ) #
    coord_quickmap() + theme_void(base_size = 6)

shr_edgelist <- case_df |> 
    group_by(shr_country, country) |> 
    summarize(n = n()) |> 
    left_join(df_attr |> select(country = nodes, comp_country = isoa2)) |> 
    select(shr_country, comp_country, n) |> 
    filter(!is.na(shr_country)) 

shr_edgelist <- shr_edgelist |> 
    left_join(df_cntr |> select(-name_long), by = c("shr_country" = "iso_a2")) |> 
    rename(coord_shr = geom) |> 
    left_join(df_cntr |> select(-name_long), by = c("comp_country" = "iso_a2")) |> 
    rename(coord_comp = geom) |> 
    mutate(coord_shr = as.character(coord_shr), coord_comp = as.character(coord_comp)) |> 
    mutate(coord_shr = str_remove_all(coord_shr, "c\\(|\\)"), 
           coord_comp = str_remove_all(coord_comp, "c\\(|\\)")) |> 
    separate(coord_shr, into = c("x", "y"), sep=", ") |> 
    separate(coord_comp, into = c("xend", "yend"), sep=", ") |> 
    mutate(across(.cols = x:yend, .fns = as.numeric)) |> 
    ## I need to add noise to the coordinates to avoid an error in geom_curve
    mutate(across(.cols = x:yend, .fns = function(x) x + rnorm(length(x), sd = 0.001))) |> 
    mutate(x = case_when(shr_country == "BM" ~ -77.957665, TRUE ~ x),
           y = case_when(shr_country == "BM" ~ 24.520330, TRUE ~ y))

shr_edgelist |> filter(is.na(xend))

world +
    geom_curve(
        data = shr_edgelist, aes(x = x,y = y, xend = xend, yend = yend, color = n), 
        alpha = 0.6, curvature = 0.1,  size = 0.1
    ) +
    scico::scale_color_scico(
        "Revenue in USD", palette = "berlin", na.value = "orange",
        guide= guide_colorbar(title.position = "top", barwidth = unit(30,"mm"),
                              barheight = unit(3, "mm"))) +
    # new_scale_color() +
    # geom_point(data = df_actors, aes(x = x, y = y, color = type), alpha = 0.75, size = 0.5) +
    # # scale_color_viridis_d(name = "Financial actor type", option = "D",
    # #                       guide = guide_legend(title.position = "top")) +
    # scale_color_brewer("Financial actor type", palette = "Paired",
    #                    guide = guide_legend(title.position = "top")) +
    # facet_wrap(~casestudy) + 
    theme_void(base_size = 6) +
    theme(legend.position = "bottom")

## Cool, add community detection to it :)

#### key actors ####
df1 |> 
    group_by(company, casestudy) |> 
    tally() |>  
    filter(casestudy != "Other") |> 
    ungroup() |> group_by(casestudy) |> 
    top_n(5) |> 
    filter(n > 1) |> 
    arrange(n) |> 
    mutate(company = str_trunc(company, width = 30, side = "right")) |> 
    ggplot(aes(n, company)) +
    geom_col(aes(fill = casestudy), show.legend = FALSE) +
    facet_wrap(~casestudy, scales = "free") +
    theme_light(base_size = 6)

ggsave(
    filename = "key_companies_per_case.png",
    plot = last_plot(),
    device = "png",
    path = "figures/",
    width = 7, height = 5,
    bg = "white", dpi = 400
)  
