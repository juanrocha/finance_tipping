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
# load data
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

# 
# dat |> 
#     ggplot(aes(ownership)) + 
#     geom_density() +
#     scale_x_log10() +
#     theme_light(base_size = 6)
# 
# ggsave(filename = "figures/ownership_distribution.png", 
#        device = "png", width = 2, height = 2)
# 

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

case_df |> pull(company) |> unique()  # 57 companies
dat |> pull(company) |> unique() # 99 companies

case_df <- case_df |> 
    left_join(dat |> rename(shr_country = country, shr_type = type)) ## 6.3k rows with countries; 3.9k without

case_df |> filter(is.na(guo_final)) |>  pull(company) |> unique() 

#remove the problematic cases for now
case_df <- case_df |> 
    filter(!is.na(guo_final)) # 
    #filter(direct_percent > 0.01) # 0.01 is Neglegible (NG) in Orbis language

unique(case_df$shareholder) %in% shr_class$shareholder |> sum()



case_df |> 
    group_by(shareholder, casestudy) |> 
    summarize(companies = n()) |> 
    filter(companies > 1) |> 
    arrange(casestudy, desc(companies)) |> 
    ungroup() |> group_by(casestudy) |> 
    mutate(casestudy = as_factor(casestudy)) |> 
    slice_head(n = 10) |> #print(n=200) |> 
    ungroup() |> 
    mutate(shareholder = as_factor(shareholder)) |> 
    mutate(shareholder = fct_reorder(shareholder, companies, sum)) |> 
    ggplot(aes(companies, shareholder)) +
    geom_col() +
    tidytext::scale_y_reordered() +
    facet_wrap(~casestudy, scales = "free_y") +
    theme_light(base_size = 6)

# ggsave(
#     filename = "top_investors_per_case.png",
#     plot = last_plot(),
#     device = "png",
#     path = "figures/",
#     width = 6, height = 5,
#     bg = "white", dpi = 400
# )

a <- case_df |> 
    group_by(shareholder, casestudy) |> 
    summarize(companies = n()) |> 
    arrange(desc(companies)) |> 
    filter(companies >= 10) |>
    ungroup() |> 
    mutate(shareholder = as_factor(shareholder)) |> 
    mutate(shareholder = fct_reorder(
        .f = shareholder, .x = companies, .fun = sum, .desc = FALSE)) |> #pull(shareholder) |> levels()
    ggplot(aes(companies, shareholder)) +
    geom_col(aes(fill = casestudy)) +
    scale_fill_brewer("Case study", palette = "Set2") +
    labs(tag = "A", y = "Top shareholders", x = "Number of companies") + 
    theme_light(base_size = 6) +
    theme(legend.position = c(0.7, 0.2), legend.key.height = unit(3,"mm"),
          legend.key.width = unit(3, "mm"))

# ggsave(
#     filename = "top_investors_type.png",
#     plot = last_plot(),
#     device = "png",
#     path = "figures/",
#     width = 3, height = 3,
#     bg = "white", dpi = 400
# )

case_df <- case_df |> 
    left_join(shr_class) |> 
    #group_by(shareholder, casestudy, shr_type) |> 
    mutate(shr_type = str_sub(class, start = 1L, end = 1L)) |> 
    mutate(shr_type = case_when(
        #is.na(shr_type) ~ "Missing",
        shr_type == "A" ~ "Agriculture, forestry and fishing",
        shr_type == "B" ~ "Mining and quarrying",
        shr_type == "C" ~ "Manufacturing",
        shr_type == "D" ~ "Electricity, gas, steam and air conditioning supply",
        shr_type == "F" ~ "Construction",
        shr_type == "G" ~ "Wholesale and retail trade",
        shr_type == "H" ~ "Transportation and storage",
        shr_type == "I" ~ "Accommodation and food service activities",
        shr_type == "J" ~ "Information and communication",
        shr_type == "K" ~ "Financial and insurance activities",
        shr_type == "L" ~ "Real estate activities",
        shr_type == "M" ~ "Professional, scientific and technical activities",
        shr_type == "N" ~ "Administrative and support service activities",
        shr_type == "O" ~ "Public administration and defence",
        shr_type == "P" ~ "Education",
        shr_type == "Q" ~ "Human health and social work activities",
        shr_type == "R" ~ "Arts, entertainment and recreation",
        shr_type == "S" ~ "Other service activities"
        #shr_type == "" ~ "I"
    )) 

case_df |> #filter(shr_type != "I") |> 
    group_by(shareholder, casestudy, shr_type) |>
    summarize(companies = n()) |> 
    arrange(desc(companies)) |> 
    filter(companies >= 10) |>
    ungroup() |> 
    mutate(shareholder = as_factor(shareholder)) |> 
    mutate(shareholder = fct_reorder(
        .f = shareholder, .x = companies, .fun = sum, .desc = FALSE)) |> #pull(shareholder) |> levels()
    ggplot(aes(companies, shareholder)) +
    geom_col(aes(fill = shr_type)) +
    scale_fill_brewer("Shareholder type",palette = "Set2", na.value = "grey50") +
    theme_light(base_size = 5) +
    theme(legend.position = c(0.7, 0.2), legend.key.height = unit(2,"mm"),
          legend.key.width = unit(2, "mm"))

### ownership figures

case_df |> 
    group_by(shareholder, casestudy) |> 
    summarize(sum_own = sum(ownership, na.rm = TRUE)) |> 
    ungroup() |> 
    slice_max(order_by = sum_own, n = 25) |> 
    mutate(shareholder = as_factor(shareholder)) |> 
    mutate(shareholder = fct_reorder(shareholder, sum_own, sum)) |> 
    ggplot(aes(sum_own, shareholder)) +
    geom_col(aes(fill = casestudy)) +
    labs(y = "Top 25 shareholders", x = "Sum of ownership") +
    scale_fill_brewer("Case studies", palette = "Set3", na.value = "grey50") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.8, 0.25))

case_df |> 
    group_by(shareholder, casestudy) |> 
    summarize(sum_own = sum(ownership, na.rm = TRUE)) |> 
    # filter(companies > 1) |> 
    #arrange(casestudy, desc(ownership)) |> 
    ungroup() |> group_by(casestudy) |> 
    mutate(casestudy = as_factor(casestudy)) |> 
    slice_head(n = 10) |> #print(n=200) |> 
    ungroup() |> 
    mutate(shareholder = as_factor(shareholder)) |> 
    #mutate(shareholder = fct_reorder(shareholder, sum_own, order)) |> 
    ggplot(aes(sum_own, shareholder)) +
    geom_col() +
    tidytext::scale_y_reordered() +
    facet_wrap(~casestudy, scales = "free_y") +
    theme_light(base_size = 5)

# ggsave(
#     filename = "top_ownership_shr_type.png",
#     plot = last_plot(),
#     device = "png",
#     path = "figures/",
#     width = 4.5, height = 4,
#     bg = "white", dpi = 400
# )

## Holdings
case_df |> 
    group_by(shareholder, casestudy) |> 
    summarize(sum_hold = sum(holdings, na.rm = TRUE)) |> 
    ungroup() |> 
    slice_max(order_by = sum_hold, n = 25) |> 
    mutate(shareholder = as_factor(shareholder)) |> 
    mutate(shareholder = fct_reorder(shareholder, sum_hold, sum)) |> 
    ggplot(aes(sum_hold, shareholder)) +
    geom_col(aes(fill = casestudy)) +
    labs(y = "Top 25 shareholders", x = "Sum of holdings") +
    scale_fill_brewer("Case studies", palette = "Set3") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.8, 0.25))

case_df |> 
    group_by(shareholder, casestudy) |> 
    summarize(sum_hold = sum(ownership, na.rm = TRUE)) |> 
    # filter(companies > 1) |> 
    arrange(casestudy, desc(sum_hold)) |> 
    ungroup() |> group_by(casestudy) |> 
    mutate(casestudy = as_factor(casestudy)) |> 
    slice_head(n = 10) |> #print(n=200) |> 
    ungroup() |> 
    mutate(shareholder = as_factor(shareholder)) |> 
    mutate(shareholder = fct_reorder(shareholder, sum_hold, sum)) |> 
    ggplot(aes(sum_hold, shareholder)) +
    geom_col() +
    tidytext::scale_y_reordered() +
    facet_wrap(~casestudy, scales = "free_y") +
    theme_light(base_size = 5)

# ggsave(
#     filename = "top_holdings_percase.png",
#     plot = last_plot(),
#     device = "png",
#     path = "figures/",
#     width = 7, height = 5,
#     #width = 4.5, height = 4,
#     bg = "white", dpi = 400
# )

### size ownership
publicomp <- read_csv2(
    file = "data/Paulas_files/002-PublicCompanies.csv", 
    locale = locale(encoding = "latin1")) |> 
    janitor::clean_names()

publicomp <- publicomp |> 
    #filter(type == "Public") |>
    mutate(company = str_to_title(company)) |> 
    select(company, market_cap_mll) |> unique()

b <- case_df |> 
    select(company, shareholder, ownership) |> 
    unique() |> 
    left_join(publicomp) |> 
    mutate(size_own = (ownership/100) * market_cap_mll) |> # skimr::skim() #induces 11 NAs
    mutate(shareholder=case_when(
        shareholder == "Archer Daniels Midland Asia-Pacific Limited" ~  "Archer Daniels Midland Company" ,
        TRUE ~ shareholder
    )) |> 
    group_by(shareholder) |> unique() |> 
    summarize(sum_own = sum(size_own, na.rm = TRUE)) |> 
    ungroup() |> #group_by(casestudy) |> 
    slice_max(order_by = sum_own, n = 25) |> 
    mutate(shareholder = as_factor(shareholder)) |> 
    mutate(shareholder = fct_reorder(shareholder, sum_own, sort)) |> 
    ggplot(aes(sum_own, shareholder)) +
    #geom_col(alpha = 0.5, fill = "goldenrod") +
    geom_col() +
    labs(x = "Size of ownership in millions US$", y = "Top shareholders", tag = "B") +
    #facet_wrap(~casestudy, scales = "free_y") +
    theme_light(base_size = 6)

# ggsave(
#     filename = "top_ownership_size.png",
#     plot = last_plot(),
#     device = "png",
#     path = "figures/",
#     width = 4.5, height = 4,
#     bg = "white", dpi = 400
# )
# 
## figure for paper
ggsave(
    filename = "Fig3_top_shareholders.png",
    plot = a+b,
    device = "png",
    path = "figures/",
    width = 6.5, height = 4,
    bg = "white", dpi = 400
)

#### network ####
# 220415: Forcing the network to be bipartite, only two of the >1800 actors show in both classes:
# - Bnp Paribas
# - Oji Holdings Corporation
inv_net <- case_df |> 
    mutate(company = case_when(
        #company == "Bnp Paribas" ~ "Bnp Paribas_c",
        company == "Oji Holdings Corporation" ~ "Oji Holdings Corporation_c",
        TRUE ~ company
    )) |> 
    select(shareholder, company, ownership, holdings, casestudy) |> unique() |> 
    filter(!is.na(shareholder)) |> 
    network(directed = TRUE, bipartite = TRUE, matrix.type = "edgelist", 
            ignore.eval=FALSE, multiple = TRUE)

#inv_net |> as.sociomatrix.sna()

# plot.network(
#     inv_net, vertex.col = "orange", edge.col = "grey",#vertex.lty = 0,
#     vertex.cex = 0.1+ log10(inv_net %v% "outdegree"))

inv_net %v% "outdegree" <- degree(inv_net, gmode = "digraph", cmode = "outdegree")
inv_net %v% "indegree" <- degree(inv_net, gmode = "digraph", cmode = "indegree")
inv_net %v% "bigshark" <- ifelse(
    inv_net %v% "outdegree" >= 20,
    inv_net %v% "vertex.names", NA
)


p1 <- ggplot(ggnetwork(inv_net, arrow.gap = 0.01, by = "casestudy"),
       aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(
        aes(color = ownership), size =0.1, alpha = 1, 
        arrow = arrow(length = unit(2, "pt"), type = "closed")) +
    scico::scale_color_scico(
        "Ownership", palette = "batlow", direction = -1,
        guide = guide_colorbar(
            barwidth = unit(2, "mm"), barheight = unit(20, "mm"))) +
    new_scale_color() +
    geom_nodes(aes(color = outdegree, size = outdegree), alpha = 0.4) +
    scale_size_area("Investments", breaks = c(1,30,60), max_size = 3) +
    scico::scale_color_scico(
        "Outdegree", palette = "romaO", direction = 1,
        guide = guide_colorbar(
            barwidth = unit(2, "mm"), barheight = unit(20, "mm"))) +
    #geom_nodes(color = "grey50", alpha = 0.2, size = 0.5) +
    #geom_nodetext_repel(aes(label = bigshark), size = 1.5, max.iter = 1000) +
    # labs(title = "Network of financial actors", 
    #      subtitle = "55 companies and 1835 investors connedted through sharehodling", 
    #      caption = "Data source: Orbis") +
    facet_wrap(~ casestudy) +
    theme_facet(base_size = 6) #Actors with names have investiments in > 20 companies
p1


# ggsave(
#     plot = p1, path = "figures/", file = "net_shareholders_cases.png", device = "png",
#     width = 5, height = 4, bg = "white", dpi = 300
# )


df_stats <- tibble(
    name = inv_net %v% "vertex.names",
    outdegree = inv_net %v% "outdegree",
    indegree = inv_net %v% "indegree",
    bet = betweenness(inv_net, gmode = "digraph", cmode = "directed", rescale = TRUE)
)

### Add a weighted metric of influence: betweenness is misleading because the network is 
### almost bipartite except for two nodes. So betweeness should be calculated on the one
### mode projections.

# 
# df_stats |> 
#     arrange(desc(outdegree)) |> 
#     filter(outdegree > 10) |> 
#     mutate(name = as_factor(name)) |> 
#     mutate(name = fct_reorder(name, outdegree)) |> 
#     ggplot(aes(outdegree, name)) +
#     geom_col(alpha = 0.5, color = "goldenrod", fill = "goldenrod", size = 0.1) +
#     labs(x = "Number of connections (shareholding)", y = "Financial actors") +
#     theme_light(base_size = 6)
# 
# ggsave(
#     plot = last_plot(), path = "figures/", file = "net_shareholders_outdegree_220404.png", 
#     device = "png",
#     width = 3, height = 4.5, bg = "white", dpi = 300
# )

### Bipartite
bip_net <- case_df |> ungroup() |> 
    mutate(company = case_when(
        #company == "Bnp Paribas" ~ "Bnp Paribas_c",
        company == "Oji Holdings Corporation" ~ "Oji Holdings Corporation_c",
        TRUE ~ company
    )) |> 
    select(shareholder, company, ownership, holdings) |> unique() |> 
    filter(!is.na(shareholder)) |> 
    network(directed = FALSE, bipartite = TRUE, matrix.type = "edgelist", 
            ignore.eval=FALSE, multiple = FALSE)

bib_mat <- bip_net |> as.sociomatrix.sna()
dim(bib_mat) # 1835 shareholders, 55 companies

shr_mat <- bib_mat %*% t(bib_mat)   # shareholders 
m <- shr_mat > 1 # threshold is at least 1, with 0 I get over 1M links and 500Mb Net
diag(m) <- 0
shr_net <- network(m, directed = FALSE)
shr_net %e% "comps" <- shr_mat
shr_net %v% "degree" <- degree(shr_net, gmode = "graph")
shr_net %v% "betweenness" <- betweenness(shr_net, gmode = "graph")

p0 <- ggplot(ggnetwork(
    # for visualization, delete isolates
    delete.vertices(shr_net, which(shr_net %v% "degree" == 0)), 
    arrow.gap = 0.01),
       aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(
        aes(color = comps, alpha = comps), size =0.1) +
    scico::scale_color_scico(
        "Companies", palette = "batlow", direction = -1,
        guide = guide_colorbar(
            barwidth = unit(2, "mm"), barheight = unit(10, "mm"))) +
    scale_alpha("Companies",range = c(0.25,1), breaks = c(10, 40),
                guide = guide_legend(keywidth = unit(2,"mm"))) + 
    new_scale_color() +
    geom_nodes(aes(color = degree, size = betweenness), alpha = 0.8) +
    scale_size_area("Betweeness", breaks = c(100,1000), max_size = 4) +
    scico::scale_color_scico(
        "Degree", palette = "romaO", direction = 1,
        guide = guide_colorbar(
            barwidth = unit(2, "mm"), barheight = unit(10, "mm"))) +
    labs(tag = "A") + 
    theme_void(base_size = 6) + 
    theme(legend.position = c(0.05, 0.5)) #c(0.05, 0.5)

p0

co_mat <- t(bib_mat) %*% bib_mat    # companies
m <- co_mat > 0
diag(m) <- 0
co_net <- network(m, directed = FALSE)
co_net %e% "shareholders" <- co_mat
co_net %v% "degree" <- degree(co_net, gmode = "graph")
co_net %v% "betweenness" <- betweenness(co_net, gmode = "graph")


shr_stats <- tibble(
    shareholder = network.vertex.names(shr_net),
    degree = shr_net %v% "degree",
    betw = shr_net %v% "betweenness"
)

co_stats <- tibble(
    company = network.vertex.names(co_net),
    degree = co_net %v% "degree",
    betw = co_net %v% "betweenness"
)

shr_stats <- case_df |> 
    select(shareholder, company, ownership, holdings, casestudy, shr_type) |> unique() |> 
    filter(!is.na(shareholder)) |> 
    group_by(shareholder, shr_type) |> 
    mutate(n = n()) |> 
    summarize(mean_own = mean(ownership),
              sd_own = sd(ownership, na.rm = TRUE)) |># arrange(desc(sd_own)) |> print(n=100)
    right_join(shr_stats) 

## problems:
shr_stats |> filter(is.na(shr_type)) |> 
    select(shareholder, degree) |> 
    arrange(desc(degree)) |> 
    print(n=61)

p1 <- shr_stats |> filter(shr_type != "I") |> 
    mutate(label = ifelse( betw > 200 | degree > 180 | mean_own > 60, shareholder, "")) |> 
    ggplot(aes(mean_own, degree)) +
    geom_point(aes(size = betw, color = shr_type), alpha = 0.5) +
    scale_color_brewer("Shareholder type", palette = "Paired") +
    scale_size("Betweenness", breaks = c(0, 100, 1000), range = c(1,5),
               guide = guide_legend(
                   direction = "horizontal", title.position = "top")) + 
    labs(x = "Mean ownership", y = "Degree", tag = "B") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.625, 0.6), legend.key.size = unit(2,"mm"),
          legend.background = element_rect(fill = alpha("white", 0.5)))

p2 <- shr_stats |> filter(shr_type != "I") |> ungroup() |> 
    slice_max(order_by = degree, n = 25) |> 
    arrange((degree)) |> 
    mutate(shareholder = as_factor(shareholder)) |> 
    ggplot(aes(degree, shareholder)) +
    geom_col(aes(fill = shr_type), show.legend = TRUE, alpha = 0.85) +
    scale_fill_brewer("Shareholder type", palette = "Paired") +
    labs(tag = "C", y = "Shareholder", x = "Degree")+
    theme_light(base_size = 6) + 
    theme(legend.position = c(0.65, 0.1), legend.key.size = unit(2,"mm"),
          legend.background = element_rect(fill = alpha("white", 0.75))) 

p3 <- shr_stats |> filter(shr_type != "I") |> ungroup() |> 
    slice_max(order_by = betw, n = 25) |> 
    arrange((betw)) |> 
    mutate(shareholder = as_factor(shareholder)) |> 
    ggplot(aes(betw, shareholder)) +
    geom_col(aes(fill = shr_type), alpha = 0.85) +
    scale_fill_brewer("Shareholder type", palette = "Paired") +
    labs(tag = "D", x = "Betweenness", y = "Shareholder")+
    theme_light(base_size = 6)  + 
    theme(legend.position = c(0.7, 0.15), legend.key.size = unit(2,"mm"),
          legend.background = element_rect(fill = alpha("white", 0.75)))   

# top <- p0 + p1 + plot_layout(widths = c(1,1))
# bot <- p2 + p3 + plot_layout(widths = c(1,1))
# 
# top/bot + plot_layout(heights = c(1,1))

(p0+p2)/(p1+p3) + plot_layout(widths = c(1.5,1.5, 1, 1))

ggsave(
    plot = (p0/p1)|(p2/p3) + plot_layout(widths = c(1.5,1.5, 1, 1)) ,
    path = "figures/", file = "shareholder_network_onemode.png",
    device = "png",
    width = 6, height = 6, bg = "white", dpi = 400
)


p0 <- ggplot(ggnetwork(
    # remove isolates 
    delete.vertices(co_net, vid = which(co_net %v% "degree" == 0 )) , 
    arrow.gap = 0.01),
       aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(
        aes(color = shareholders, alpha = shareholders), size =0.1) +
    scico::scale_color_scico(
        "Shareholders", palette = "batlow", direction = -1,
        guide = guide_colorbar(
            barwidth = unit(2, "mm"), barheight = unit(10, "mm"))) +
    scale_alpha("Sharenolders",range = c(0.25,1), breaks = c(10, 60),
                guide = guide_legend(keywidth = unit(2,"mm"))) +
    new_scale_color() +
    geom_nodes(aes(color = degree, size = betweenness), alpha = 0.4) +
    scale_size_area("Betweeness", breaks = c(1,5), max_size = 4) +
    scico::scale_color_scico(
        "Degree", palette = "romaO", direction = 1,
        guide = guide_colorbar(
            barwidth = unit(2, "mm"), barheight = unit(10, "mm"))) +
    labs(tag = "A") +
    theme_void(base_size = 6) + theme(legend.position = c(0.05, 0.5))

co_stats <- df1 |> select(company, no_workers, type, turnover) |>
    filter(type == "Public") |> unique() |>
    arrange(company) |>  #print(n=122)
    group_by(company, no_workers) |> 
    summarize(turnover = max(turnover, na.rm = TRUE)) |> 
    ungroup() |> 
    right_join(co_stats)  #print(n=122)
    
p1 <- co_stats |> 
    ggplot(aes(degree, betw)) +
    geom_point(aes(size = turnover, color = turnover), alpha = 0.5, position = 'jitter') +
    scico::scale_color_scico(
        palette = "vikO", na.value = "grey50", n.breaks = 3,
        guide = guide_colorbar(
            title.position = "top", barwidth = unit(25,"mm"), barheight = unit(2,"mm"))) +
    scale_size(breaks = c(5000,55000), range = c(1,5),
               guide = guide_legend(title.position = "top"))+
    labs(x = "Degree", y = "Betweenness", tag = "B") +
    theme_light(base_size = 6) +
    theme(legend.direction = "horizontal", legend.position = c(0.25,0.8))

p2 <- co_stats |> 
    slice_max(order_by = betw, n = 15) |> 
    arrange((betw)) |> 
    mutate(company = as_factor(company)) |> 
    ggplot(aes(betw, company)) +
    geom_col(aes(fill = turnover), alpha = 0.85) +
    scale_fill_viridis_c("Turnover", option = "D", n.breaks = 5) +
    labs(tag = "C", x = "Betweenness", y = "Company")+
    theme_light(base_size = 6)  + 
    theme(legend.position = c(0.85, 0.2), legend.key.height = unit(3.5,"mm"),
          legend.key.width = unit(2,"mm"), 
          legend.background = element_rect(fill = alpha("white", 0.5)))

p3 <- co_stats |> 
    slice_max(order_by = betw, n = 15) |> 
    arrange((degree)) |> 
    mutate(company = as_factor(company)) |> 
    ggplot(aes(degree, company)) +
    geom_col(aes(fill = turnover), alpha = 0.85, show.legend = FALSE) +
    scale_fill_viridis_c("Turnover", option = "D") +
    labs(tag = "D", x = "Degree", y = "Company")+
    theme_light(base_size = 6) 


(p0 + p2) / (p1 + p3) 

# ggsave(
#     plot = (p0 + p2) / (p1 + p3) ,
#     path = "figures/", file = "company_network_onemode.png",
#     device = "png",
#     width = 6, height = 5, bg = "white", dpi = 500
# )

write_csv(shr_stats, file = "data/shr_stats.csv")

#### Network map ####

load("data/map_data.RData")

df_map <- left_join(
    dat |> 
    select(company, shareholder, country_shr = country, type, ownership, op_revenue) |>
        unique() |> 
        filter(!is.na(country_shr), !shareholder %in% c("Public", "Self Owned"), !is.na(country_shr)),
    df1 |> filter(casestudy != "Other" , type != "Private", !is.na(country))  |> 
        select(company, country_comp = country, commodity, casestudy) |> unique() 
) |> 
    filter(!is.na(commodity), !is.na(country_comp))

df_map <- df_map |> 
    left_join(publicomp)


skimr::skim(df_map)

wb_countries <- read_csv("~/Documents/Projects/DATA/WorldBank/WDI_csv/WDICountry.csv") |> 
    janitor::clean_names() 


data(world)

df_map$country_shr |> unique() %in% wb_countries$x2_alpha_code ## Taiwan

df_cntr <- world |> 
    #filter(!is.na(pop)) |> # remove non-countries aggregations
    st_centroid(of_largest = TRUE) |> 
    select(iso_a2, name_long, geom)

df_cntr |> arrange((name_long)) |> print(n=200)

df_map <- df_map |> 
    mutate(country_shr = case_when(country_shr == "TW" ~ "CN", TRUE ~ country_shr)) |> 
    left_join(df_cntr |> select(country_shr = iso_a2, coord_shr = geom))

df_map |> select(country_shr, coord_shr) |> unique() |> print(n=50)

## Correct manually coordinates for missing places (non-countries)
df_map$coord_shr[df_map$country_shr == "KY"] <- st_point(c( -80.5470, 19.4282))
df_map$coord_shr[df_map$country_shr == "SG"] <- st_point(c(103.813346, 1.351065))
df_map$coord_shr[df_map$country_shr == "BM"] <- st_point(c(-64.751305 , 32.35904))
df_map$coord_shr[df_map$country_shr == "HK"] <- st_point(c(114.233534, 22.382964))
df_map$coord_shr[df_map$country_shr == "BM"] <- st_point(c(57.544221, -20.318906))
df_map$coord_shr[df_map$country_shr == "MU"] <- st_point(c(-64.751305 , 32.35904))
df_map$coord_shr[df_map$country_shr == "VG"] <- st_point(c(-64.609855, 18.437927))


df_map$country_comp |> unique() %in% df_cntr$name_long

cnt_list <- unique(df_map$country_comp) 
cnt_list[!cnt_list %in% df_cntr$name_long]

## correct names manually before join
df_map$country_comp[df_map$country_comp == "UK"] <- "United Kingdom"
df_map$country_comp[df_map$country_comp == "South Korea"] <- "Republic of Korea"
df_map$country_comp[df_map$country_comp == "USA"] <- "United States"
df_map$country_comp[df_map$country_comp == "Ivory Coast"] <- "Côte d'Ivoire"

df_map <- df_map |> 
    left_join(df_cntr |> select(country_comp = name_long, coord_comp = geom))

df_actors <- bind_rows(
    df_map |> select(actor = company, coord = coord_comp, casestudy) |> unique(),
    df_map |> select(actor = shareholder, coord = coord_shr, type, casestudy) |> unique()
)

df_map <- df_map |> 
    mutate(coord_shr = as.character(coord_shr), coord_comp = as.character(coord_comp)) |> 
    mutate(coord_shr = str_remove_all(coord_shr, "c\\(|\\)"), 
           coord_comp = str_remove_all(coord_comp, "c\\(|\\)")) |> 
    separate(coord_shr, into = c("x", "y"), sep=", ") |> 
    separate(coord_comp, into = c("xend", "yend"), sep=", ") |> 
    mutate(across(.cols = x:yend, .fns = as.numeric)) |> 
    ## I need to add noise to the coordinates to avoid an error in geom_curve
    mutate(across(.cols = x:yend, .fns = function(x) x + rnorm(length(x), sd = 0.001))) 

df_actors <- df_actors |> 
    mutate(coord = as.character(coord) |> str_remove_all( "c\\(|\\)")) |> 
    separate(coord, into = c("x", "y"), sep=", ") |> 
    mutate(across(x:y, as.numeric)) |> 
    mutate(type = case_when(
        is.na(type) ~ "Company",
        type == "A" ~ "Insurance company",
        type == "B" ~ "Bank",
        type == "C" ~ "Corporate companies",
        type == "E" ~ "Mutual and pension fund",
        type == "F" ~ "Financial company",
        type == "J" ~ "Foundation, research institute",
        type == "L" ~ "Unnamed shareholders",
        type == "P" ~ "Private equity firms",
        type == "S" ~ "Public authorities, states, government",
        type == "V" ~ "Venture capital",
        type == "Y" ~ "Hedge fund",
        type == "" ~ "I"
    )) |> filter(type != "I") # an individual

df_map <- df_map |> 
    mutate(size_own = (ownership /100)*market_cap_mll)

df_actors <- df_actors |>
    mutate(type2 = case_when(type == "Company" ~ "Company", TRUE ~ "Investors")) 

   # print(n=200)

## Cant' do networks on sf, need to go down to bare ggplot
## old way
world <- ggplot(map_data("world"), aes(x = long, y = lat)) +
    geom_polygon(aes(group = group), color = "grey65",
                 fill = "#f9f9f9", size = 0.05) +
    #coord_map(projection = "mercator" ) #
    coord_quickmap() + theme_void(base_size = 6)

world +
    geom_curve(
        data = df_map, aes(x = x,y = y, xend = xend, yend = yend, color = size_own), 
         alpha = 0.85, curvature = 0.1,  size = 0.05
    ) +
    # scico::scale_color_scico( trans = "log1p",
    #     "Size of ownership in millions US$", palette = "berlin", na.value = "black",
    #     guide= guide_colorbar(title.position = "top", barwidth = unit(30,"mm"),
    #                           barheight = unit(3, "mm"))) +
    scale_color_viridis_c( trans = "log10",
        name = "Size of ownership\nin millions US$", option = "D", na.value = "black",
        guide= guide_colorbar(title.position = "top", barwidth = unit(3,"mm"),
                             barheight = unit(30, "mm"))) +
    new_scale_color() +
    geom_jitter(data = df_actors, aes(x = x, y = y, color = type2),
               alpha = 0.5, size = 0.35, width = 5, height = 5) +
    
    scale_color_brewer("", palette = "Set1",
                       guide = guide_legend(title.position = "top")) +
    facet_wrap(~casestudy) + 
    theme_void(base_size = 6) +
    theme(legend.position = "right")

ggsave(
    plot = last_plot(), file = "network_map_cases_jittered.pdf", path = "figures/", device = "pdf",
    width = 7, height = 5, dpi = 1200, bg = "white"
)


save(df_map, df_actors, file = "data/map_data.RData")


### Asnwer Paula's question on number of companies
case_df |>
    select(company, casestudy, type) |> 
    unique() |> 
    group_by(casestudy, type) |>
    summarize(n = n()) 

df1 |> 
    group_by(casestudy, type) |> 
    unique() |> 
    summarize(n = n()) |> 
    pivot_wider(names_from = type, values_from = n) |> 
    mutate(Total = sum(Private, Public, `NA`, na.rm = TRUE))

dat |> 
    pull(company) |> 
    unique() |> 
    length()

df1 |> pull(company) |> unique() |> length()
