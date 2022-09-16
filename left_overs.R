#### Network of countries per case study ####
# This first analysis merge all commodities into "geographies". Subsequent analysis 
# could separate by commodity and see if it holds
# Exclude nodes that are not countries:

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
