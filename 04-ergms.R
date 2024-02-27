## exponential random graph models of financial actors
library(tidyverse)
library(sna)
library(ergm)
library(broom)
library(ggnetwork)
library(patchwork)
library(ggnewscale)
library(tictoc)

#alternatively: it contains the case study column needed for spliting.
# load data
load("data/investors_cleaned.RData")
load("data/casestudies.RData")

no_country <- c("Central Europe", "Eastern Europe", "Middle East", "Southern Europe", "Europe", "Fejér", "Taiwan")


#### Network countries sharing companies: One single network ####
net_edgelist <-  df1 %>% 
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
    ungroup() 

net_countries <- net_edgelist |> #unique() |> 
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
net_countries %e% "chanlog" <- log1p(net_countries %e% "channel")

#### ergm: binary ####
# null model
# f0 <- ergm(net_countries ~ edges)
# # governance model
# tic()
# f1 <- ergm(
#     net_countries ~ edges + diff("corruption") + diff("gini") +
#         diff("gov_effectiveness") +  diff("pol_stability") +
#         diff("regulatory_qual") + diff("rule_law") + diff("voice_acc"))
# toc()
# # governance + inequality
# tic()
# f2 <- ergm(
#     net_countries ~ edges + diff("corruption") + diff("gini") +
#         diff("gov_effectiveness") +  diff("pol_stability") +
#         diff("regulatory_qual") + diff("rule_law") + diff("voice_acc") +
#         nodeicov("corruption") + nodeicov("gov_effectiveness") +
#         nodeicov("pol_stability" ) +
#         nodeicov("regulatory_qual" ) + nodeicov("rule_law") + 
#         nodeicov("gini"))
# toc()

# g+i+structure
# tic()
# f3 <- ergm(
#     net_countries ~ edges + diff("corruption") + diff("gini") +
#         diff("gov_effectiveness") +  diff("pol_stability") +
#         diff("regulatory_qual") + diff("rule_law") + diff("voice_acc") +
#         nodeicov("corruption") + nodeicov("gov_effectiveness") +
#         nodeicov("pol_stability" ) +
#         nodeicov("regulatory_qual" ) + nodeicov("rule_law") + 
#         nodeicov("gini") +  mutual + dgwnsp(type ="ISP") 
# )
# toc() # 91660s = 25.4hrs


summary(f1)
load("simple_ergms.RData")


#### ergm: weighted ####
# library(ergm.count)
# library(tictoc)
# tic()
# f0 <- ergm(
#     net_countries ~ nonzero + sum, 
#     response = 'channel', reference = ~Poisson
#     # constraints = ~bd(maxin = 17, maxout = 50),
#     # constraints = ~degreedist
#     # control = snctrl(
#     #     MCMC.prop = ~ sparse,
#     #     #init.method = "MPLE",
#     #     MPLE.constraints.ignore = TRUE,
#     #     init.MPLE.samplesize = 5e7,
#     #     SAN.nsteps = 5e7,
#     #     SAN.prop = ~ sparse
#     #)
# ) # null model
# toc() # 7s
# # Governance models
# tic()
# f1 <- ergm(
#     net_countries ~ nonzero + sum + diff("corruption", form = "sum") + diff("gov_effectiveness", form = "sum") +
#         diff("pol_stability", form = "sum") + diff("regulatory_qual", form = "sum") + 
#         diff("rule_law", form = "sum") + diff("voice_acc", form = "sum"), 
#     response = 'channel', reference = ~Poisson)
# toc() # 46s
# 
# tic()
# f2 <-  ergm(
#     net_countries ~ nonzero + sum + diff("corruption", form = "sum") +  
#         diff("rule_law", form = "sum") + diff("voice_acc", form = "sum"), 
#     response = 'channel', reference = ~Poisson )
# toc() #110s
# 
# # governance and inequality models
# tic()
# f3 <-  ergm(
#     net_countries ~ nonzero + sum + diff("corruption", form = "sum") + 
#         diff("gov_effectiveness", form = "sum") +
#         diff("pol_stability", form = "sum") + diff("regulatory_qual", form = "sum") + 
#         diff("rule_law", form = "sum") + diff("voice_acc", form = "sum") +
#         diff("gini", form = "sum"), 
#     response = 'channel', reference = ~Poisson )
# toc() #107
# 
# tic()
# f4 <-  ergm(
#     net_countries ~ nonzero + sum + diff("corruption", form = "sum") + 
#         diff("gov_effectiveness", form = "sum") +
#         diff("pol_stability", form = "sum") + diff("regulatory_qual", form = "sum") + 
#         diff("rule_law", form = "sum") + diff("voice_acc", form = "sum") +
#         diff("gini", form = "sum" )+
#         nodeicov("corruption", form = "sum" ) + nodeicov("gov_effectiveness",form = "sum" ) +
#         nodeicov("pol_stability", form = "sum" ) +
#         nodeicov("regulatory_qual", form = "sum" ) + nodeicov("rule_law", form = "sum" ) + 
#         nodeocov("corruption", form = "sum" ) +
#         nodeocov("gov_effectiveness", form = "sum" ) + nodeocov("pol_stability", form = "sum" ) +
#         nodeocov("regulatory_qual", form = "sum" ) + nodeocov("rule_law", form = "sum" ) + 
#         nodeicov("gini", form = "sum" ) + nodeocov("gini", form = "sum" ), 
#     response = 'channel', reference = ~Poisson )
# toc()

fits <- list(f0, f1, f2)
fit_name <- c("null", "difference","full")

df_stats <- tibble(
    model = fit_name,
    case = names(fits),
    logLik = map_dbl(fits, logLik),
    AIC = map_dbl(fits, AIC)
)

#### structural models ####
#f5 <- ergm(net_countries ~ edges + triadcensus())

fits <- fits |> map(tidy, exponentiate = FALSE) 
fits <- map2(fits, fit_name, function(x,y) {x$model <- y; return(x)})
#fits <- map2(fits, names(fits), function(x,y) {x$case <- y; return(x)})

fig5c <- fits |> 
    bind_rows() |> 
    mutate(p_value = case_when(
        p.value < 0.001 ~ "p < 0.001",
        p.value > 0.001 & p.value <= 0.05 ~ "p < 0.05",
        p.value > 0.05 ~ "p > 0.05")) |> 
    mutate(term = case_when(
        term == "edges" ~ "Edges",
        term == "diff.t-h.corruption" ~ "Difference in corruption",
        term == "diff.t-h.gini" ~ "Difference in inequality (Gini)",
        term == "diff.t-h.gov_effectiveness" ~ "Difference in governance effectiveness",
        term == "diff.t-h.pol_stability" ~ "Difference in political stability",
        term == "diff.t-h.regulatory_qual" ~ "Difference in regulatory quality",
        term == "diff.t-h.rule_law" ~ "Difference in rule of law",
        term == "diff.t-h.voice_acc" ~ "Difference in voice accountability",
        term == "nodeicov.corruption" ~ "Corruption",
        term == "nodeicov.gov_effectiveness" ~ "Government effectiveness",
        term == "nodeicov.pol_stability" ~ "Political stability",
        term == "nodeicov.regulatory_qual" ~"Regulatory quality",
        term == "nodeicov.rule_law" ~ "Rule of law",
        term == "nodeicov.gini" ~ "Inequality (Gini)")) |> 
    mutate(model = as_factor(model)) |> 
    mutate(term = fct_relevel(term, "Edges")) |>
    mutate(term = fct_rev(term)) |> 
    ggplot(aes(estimate, term)) + 
    geom_vline(aes(xintercept = 0), color = "gray50", linetype = 2, size = 0.1) +
    geom_point(aes(color = p_value), size = 0.8) +
    geom_errorbarh(
        aes(xmin = estimate - std.error, xmax = estimate + std.error, color = p_value),
        height = 0.5, size = 0.15) + 
    #geom_text(aes( label = logLik), data = df_stats) +
    scale_color_brewer(
        "p value", palette = "Dark2",
        guide = guide_legend(title.position = "top", keywidth = unit(2,"mm"), 
                             keyheight = unit(2, "mm"))) +
    facet_wrap(~model, ncol = 5) +
    labs(tag = "C", x = "Odds of forming a link in the network", y = "Network terms: node or edge attributes") +
    theme_linedraw(base_size = 6) +
    theme(legend.position = c(0.15,0.25))
fig5c
# b <- ggplot(df_stats, aes(logLik, model)) +
#     geom_col() +
#     labs(tag = "B") +
#     theme_light(base_size = 7)
# c <- ggplot(df_stats, aes(AIC, model)) +
#     geom_col() +
#     labs(tag = "C") +
#     theme_light(base_size = 7)


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

# df_attr |> 
#     select(-nodes, -isoa2, -country_code) |> 
#     GGally::ggpairs() +
#     theme_light(base_size = 10)
# 
# ## correlations
# ggsave(
#     filename = "correlations.png",
#     plot = last_plot(),
#     device = "png",
#     path = "figures/",
#     width = 7, height = 7,
#     bg = "white", dpi = 400
# )

fig5b <- df_attr |> 
    ggplot(aes(indegree, outdegree)) +
    geom_point(aes(color = `Government Effectiveness`), size = 4, alpha = 0.9) +
    geom_text(aes(label = isoa2), color = "white", size = 2) + 
    geom_abline(slope = 1, intercept = 0, color = "red") +
    scico::scale_color_scico(
        palette = "berlin",
        guide = guide_colourbar(title.position = "top", direction = "horizontal")) +
    labs(tag = "B") +
    theme_light(base_size = 5) +
    theme(legend.position = c(0.8, 0.87), legend.key.height = unit(0.15, "cm"),
          legend.key.width = unit(0.4, "cm"))
fig5b

# ggsave(
#     filename = "country_netmodels_220126.png",
#     plot = a/( b + c + d) ,
#     device = "png",
#     path = "figures/",
#     width = 7, height = 5,
#     bg = "white", dpi = 400
# )


# ggsave(
#     filename = "net_stats.png",
#     plot = last_plot(),
#     device = "png",
#     path = "figures/",
#     width = 4, height = 3,
#     bg = "white", dpi = 400
# )              


## map for companies country networks



## To-do: make a map with communities instead of b and c panels. Put the model stats on one fig for SM


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
    shr_net ~ edges + diff("corruption") + diff("gini") +
        diff("gov_effectiveness") +  diff("pol_stability") +
        diff("regulatory_qual") + diff("rule_law") + diff("voice_acc"))
# governance and inequality
s2 <-  ergm(
    shr_net ~ edges + diff("corruption") + diff("gini") +
        diff("gov_effectiveness") +  diff("pol_stability") +
        diff("regulatory_qual") + diff("rule_law") + diff("voice_acc") +
        nodeicov("corruption") + nodeicov("gov_effectiveness") +
        nodeicov("pol_stability" ) +
        nodeicov("regulatory_qual" ) + nodeicov("rule_law") + 
        nodeicov("gini"))

# governance and inequality models + structure
tic()
s3 <-  ergm(
    shr_net ~ edges + diff("corruption") + diff("gini") +
        diff("gov_effectiveness") +  diff("pol_stability") +
        diff("regulatory_qual") + diff("rule_law") + diff("voice_acc") +
        nodeicov("corruption") + nodeicov("gov_effectiveness") +
        nodeicov("pol_stability" ) +
        nodeicov("regulatory_qual" ) + nodeicov("rule_law") + 
        nodeicov("gini") +  mutual + intransitive)
toc() # 722s

# s4 <-  ergm(
#     shr_net ~ edges +  diff("corruption") + diff("gini") +
#         diff("gov_effectiveness") +  diff("pol_stability") +
#         diff("regulatory_qual") + diff("rule_law") + diff("voice_acc") +
#         nodeicov("corruption") + nodeicov("gov_effectiveness") +
#         nodeicov("pol_stability" ) +
#         nodeicov("regulatory_qual" ) + nodeicov("rule_law") + 
#         nodeicov("gini") )

#save(f0,f1,f2,f3, net_countries, shr_net, s0, s1, s2, file = "simple_ergms.RData")


fits2 <- list(s0, s1, s2)
fit_name2 <- c("null", "difference","full")

df_stats2 <- tibble(
    model = fit_name2,
    case = names(fits2),
    logLik = map_dbl(fits2, logLik),
    AIC = map_dbl(fits2, AIC)
)


df_stats2

### Produce the same figures but for stakeholder networks

fits2 <- fits2 |> map(tidy, exponentiate = FALSE) 
fits2 <- map2(fits2, fit_name2, function(x,y) {x$model <- y; return(x)})
#fits <- map2(fits, names(fits), function(x,y) {x$case <- y; return(x)})

fig6c <- fits2 |> 
    bind_rows() |> 
    mutate(p_value = case_when(
        p.value < 0.05 ~ "p < 0.05",
        p.value > 0.05 & p.value <= 0.1 ~ "p < 0.1",
        p.value > 0.1 ~ "p > 0.1")) |> 
    mutate(term = case_when(
        term == "edges" ~ "Edges",
        term == "diff.t-h.corruption" ~ "Difference in corruption",
        term == "diff.t-h.gini" ~ "Difference in inequality (Gini)",
        term == "diff.t-h.gov_effectiveness" ~ "Difference in governance effectiveness",
        term == "diff.t-h.pol_stability" ~ "Difference in political stability",
        term == "diff.t-h.regulatory_qual" ~ "Difference in regulatory quality",
        term == "diff.t-h.rule_law" ~ "Difference in rule of law",
        term == "diff.t-h.voice_acc" ~ "Difference in voice accountability",
        term == "nodeicov.corruption" ~ "Corruption",
        term == "nodeicov.gov_effectiveness" ~ "Government effectiveness",
        term == "nodeicov.pol_stability" ~ "Political stability",
        term == "nodeicov.regulatory_qual" ~"Regulatory quality",
        term == "nodeicov.rule_law" ~ "Rule of law",
        term == "nodeicov.gini" ~ "Inequality (Gini)")) |> 
    mutate(model = as_factor(model)) |> 
    mutate(term = fct_relevel(term, "Edges")) |>
    mutate(term = fct_rev(term)) |> 
    ggplot(aes(estimate, term)) + 
    geom_vline(aes(xintercept = 0), color = "gray50", linetype = 2, size = 0.1) +
    geom_point(aes(color = p_value), size = 0.8) +
    geom_errorbarh(
        aes(xmin = estimate - std.error, xmax = estimate + std.error, color = p_value),
        height = 0.5, size = 0.15) + 
    #geom_text(aes( label = logLik), data = df_stats) +
    scale_color_brewer(
        "p value", palette = "Dark2",
        guide = guide_legend(title.position = "top", keywidth = unit(2,"mm"), 
                             keyheight = unit(2, "mm"))) +
    facet_wrap(~model, ncol = 5) +
    labs(tag = "C") +
    theme_linedraw(base_size = 6) +
    theme(legend.position = c(0.15,0.25))
fig6c

d2 <- ggplot(df_stats2, aes(logLik, model)) +
    geom_col() +
    labs(tag = "B") +
    theme_light(base_size = 7)

c <- ggplot(df_stats2, aes(AIC, model)) +
    geom_col() +
    labs(tag = "C") +
    theme_light(base_size = 7)

fig6b <- df_attr2 |> 
    ggplot(aes(indegree, outdegree)) +
    geom_point(aes(color = `Government Effectiveness`), size = 4, alpha = 0.9) +
    geom_text(aes(label = nodes), color = "white", size = 2) + 
    geom_abline(slope = 1, intercept = 0, color = "red") +
    scico::scale_color_scico(
        palette = "berlin",
        guide = guide_colourbar(title.position = "top", direction = "horizontal")) +
    labs(tag = "B") +
    theme_light(base_size = 5) +
    theme(legend.position = c(0.3, 0.87), legend.key.height = unit(0.15, "cm"),
          legend.key.width = unit(0.4, "cm"))
fig6b
# ggsave(
#     filename = "country_shareholder_netmodels_220505.png",
#     plot = a/( b+c + d) ,
#     device = "png",
#     path = "figures/",
#     width = 7, height = 5,
#     bg = "white", dpi = 400
# )


## Cool, add community detection to it :)
#library(igraph)

shr_edgelist <- case_df |> 
    group_by(shr_country, country) |> 
    summarize(n = n()) |> 
    left_join(df_attr |> select(country = nodes, comp_country = isoa2)) |> 
    select(shr_country, comp_country, n) |> 
    filter(!is.na(shr_country)) 

shr_igraph <- shr_edgelist |> 
    select(1:3) |> 
    igraph::graph_from_data_frame() 

ctr_igraph <- net_edgelist |> 
    igraph::graph_from_data_frame()

memb <- ctr_igraph |> 
    igraph::cluster_walktrap(weights = igraph::edge_attr(ctr_igraph)$n)

imc <- shr_igraph |> 
    igraph::cluster_walktrap(weights = igraph::edge_attr(shr_igraph)$n)

# ## only one cluster
# membership(imc)
# communities(imc)
# plot(imc, shr_igraph )

community <- tibble(
    nodes = names(igraph::membership(imc)),
    comm = igraph::membership(imc)
)

community2 <- tibble(
    nodes = names(igraph::membership(memb)),
    comm2 = igraph::membership(memb)
)

## All communities above 6 are of only one member:
community2 <- community2 |> 
    mutate(comm2 = as.numeric(comm2)) |> 
    mutate(comm2 = case_when(comm2 > 5 ~ 6, TRUE ~ comm2))

df_attr2 <- df_attr2 |> 
    left_join(community)

df_attr <- df_attr |> 
    #select(-comm2) |> 
    left_join(community2)

shr_net %v% "community" <- as.numeric(df_attr2$comm)


### Pull tables for editors:
case_df |>  # this is the shareholder network
    group_by(shr_country, country) |> 
    summarize(n = n()) |> 
    left_join(df_attr |> select(country = nodes, comp_country = isoa2)) |> 
    select(shr_country, comp_country, n) |> 
    filter(!is.na(shr_country)) |> 
    left_join(df_attr2 |> select(shr_country = nodes, comm)) |> 
    filter(shr_country == "NO") |> 
    arrange(desc(n))
    write_csv(file = "data/table_fig_sm_shareholders.csv")

net_edgelist |> 
    left_join(df_attr |> select(origin = nodes, comm2)) |> 
    filter(origin == "Norway") |> 
    arrange(desc(channel))
    write_csv(file = "data/table_fig_5_companies-communities.csv")

df_attr |> 
    select(nodes, starts_with("comm")) |> 
    write_csv("data/table_for_bianca.csv")

## maps
library(sf)
library(spData)
data(world)

world <- left_join(world, community, by = c("iso_a2" = "nodes"))
world <- left_join(world, df_attr, by = c("iso_a2" = "isoa2"))

prob <- df_attr$isoa2[!df_attr$isoa2 %in% world$iso_a2] # easier to join with iso codes, but still a few missing
community2$nodes[!community2$nodes %in% world$name_long]

df_attr |> filter(isoa2 %in% prob)

# df_cntr <- world |> 
#     #filter(!is.na(pop)) |> # remove non-countries aggregations
#     st_centroid(of_largest = TRUE) |> 
#     select(iso_a2, name_long, geom)

world_map <- ggplot(world) +
    geom_sf(aes(fill = as.factor(comm)), size = 0.1, alpha = 0.3) +
    scale_fill_brewer(
        "Communities",palette = "Set1", na.value = "gray95", direction = -1,
        guide = guide_legend(title.position = "top", keywidth = unit(2,"mm"), 
                             keyheight = unit(2, "mm"))) +
    lims(y = c(-55, 90)) + labs(tag = "A") +
    theme_void(base_size = 6) +
    theme(legend.position = c(0.2, 0.3))
world_map

(world_map / fig5b) / fig5c + plot_layout(heights = c(1.5,1,1))
#world_map/(fig5b+fig5c) + plot_layout(heights = c(1.5,1))
ggsave(
    plot = (world_map / fig5b) / fig5c + plot_layout(heights = c(1.5,1,1)),
    filename = "figures/fig4_shr_net_map.pdf",
    device = "pdf", width = 7, height = 5, dpi = 400, bg = "white"
)

a <- df_stats |> 
    mutate(model = as_factor(model) |> fct_rev()) |> 
    pivot_longer(cols = c("logLik", "AIC"), names_to = "metric", values_to = "value") |> 
    ggplot(aes(value, model)) +
    geom_col() +
    facet_grid(~metric, scales = "free") +
    labs(tag = "A") +
    theme_light(base_size = 6)

b <- df_stats2 |> 
    mutate(model = as_factor(model) |> fct_rev()) |> 
    pivot_longer(cols = c("logLik", "AIC"), names_to = "metric", values_to = "value") |> 
    ggplot(aes(value, model)) +
    geom_col() +
    facet_grid(~metric, scales = "free") +
    labs(tag = "B") +
    theme_light(base_size = 6)

ggsave(
    plot = a+b,
    filename = "sm_model_performance.png",
    path = "figures/", dpi = 300, device = "png",
    width = 6, height = 2, bg = "white"
)


# shr_edgelist <- shr_edgelist |> 
#     left_join(df_cntr |> select(-name_long), by = c("shr_country" = "iso_a2")) |> 
#     rename(coord_shr = geom) |> 
#     left_join(df_cntr |> select(-name_long), by = c("comp_country" = "iso_a2")) |> 
#     rename(coord_comp = geom) |> 
#     mutate(coord_shr = as.character(coord_shr), coord_comp = as.character(coord_comp)) |> 
#     mutate(coord_shr = str_remove_all(coord_shr, "c\\(|\\)"), 
#            coord_comp = str_remove_all(coord_comp, "c\\(|\\)")) |> 
#     separate(coord_shr, into = c("x", "y"), sep=", ") |> 
#     separate(coord_comp, into = c("xend", "yend"), sep=", ") |> 
#     mutate(across(.cols = x:yend, .fns = as.numeric)) |> 
#     ## I need to add noise to the coordinates to avoid an error in geom_curve
#     mutate(across(.cols = x:yend, .fns = function(x) x + rnorm(length(x), sd = 0.001))) |> 
#     # Add coordinates of Bahamas
#     mutate(x = case_when(shr_country == "BM" ~ -77.957665, TRUE ~ x),
#            y = case_when(shr_country == "BM" ~ 24.520330, TRUE ~ y))
# 
# shr_edgelist |> filter(is.na(xend))
# 
# m1 <- world_map +
#     geom_curve(
#         data = shr_edgelist, aes(x = x,y = y, xend = xend, yend = yend, color = n), 
#         curvature = 0.1,  size = 0.1
#     ) +
#     scico::scale_color_scico(
#         "Financial actors", 
#         palette = "bamako", na.value = "orange", direction = 1,
#         guide= guide_colorbar(title.position = "top", barwidth = unit(30,"mm"),
#                               barheight = unit(3, "mm"))) +
#     labs(tag = "C") +
#     theme_void(base_size = 6) +
#     theme(legend.position = "bottom")
# 
# 
# m1 / (d+a) + plot_layout(heights = c(2,1))




# ggplot(ggnetwork(shr_net), aes(x = x, y = y, xend = xend, yend = yend)) +
#     geom_edges(aes(color = n), arrow = arrow(length = unit(4, "pt"), type = "open"),
#                alpha = 0.4, size = 0.25) +
#     scale_color_viridis_c("Shareholders", option = "C") +
#     new_scale_color() +
#     geom_nodes(aes(color = as.factor(community)), alpha = 0.3) +
#     scale_color_brewer(
#         "Communities",palette = "Set1", na.value = "gray95", direction = -1,
#         guide = guide_legend(title.position = "top", keywidth = unit(2,"mm"), 
#                              keyheight = unit(2, "mm"))) +
#     theme_void()

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


### edits requests:
prbl <- fig5b$data |>
    select(nodes, indegree, outdegree ) |>
    filter(outdegree < 10, indegree < 12) |> 
    arrange(indegree) |> 
    split(~indegree) |> 
    map(function(x) pull(x, nodes)) 

for (i in 1:length(prbl)){
    prbl[[i]] |> cat(sep = ";")
}
prbl[[1]] |> cat(sep = "; ")
prbl[[1]] |> str_flatten(collapse = "; ")
prbl |> map(function(x) str_flatten(x, collapse = "; ", last = " and "))
