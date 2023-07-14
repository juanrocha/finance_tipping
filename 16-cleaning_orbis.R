library(tidyverse)
library(tictoc)
library(patchwork)
# load datasets


load("data/boats_owners.Rda")
load("data/marine_shareholders.RData")

df_boats

owners <- df_boats |>
    filter(!is.na(reg_owner)) |> 
    pull(reg_owner) |> unique()

guo_list |> length()
revenues |> length()
shr_list |> length()

guo_list[[1]]
revenues[[1]]
shr_list[[1]]

## First I need to clean the list object and coerce to df
guo_list[[1]]

guo_list[[1]][[1]]$X1[2] |> 
    str_remove_all("\\n|\\t") |> 
    #str_extract("Orbis ID: \\d{1,10}") |> 
    str_extract("BvD ID: \\w{1,12}")


#### Create functions ####
extract_guo <- function(x){
    
    if(is.null(x))
        return(out <- tibble(
            orbis_id = NA,
            BvD_id = NA,
            orbis_name = NA,
            location = NA,
            guo = NA))
    
    else
        orbis_id <- x[[1]]$X1[2] |> 
            str_remove_all("\\n|\\t") |> 
            str_extract("Orbis ID: \\d{1,10}")
    
    BvD_id <- x[[1]]$X1[2] |> 
        str_remove_all("\\n|\\t") |> 
        str_extract("BvD ID: \\w{1,12}")
    
    txt <- x[[1]]$X2[1] |> 
        str_split(pattern = " \n\t\t\t\t\n\t\t\t\t") |> 
        unlist()
    
    location <- txt[2]
    orbis_name <- txt[1] |> str_remove("0\n\t\t\t\t")
    
    guo <- x[[1]]$X3[2]
    
    out <- tibble(
        orbis_id = orbis_id,
        BvD_id = BvD_id,
        orbis_name = orbis_name,
        location = location,
        guo = guo)
    
    
    return(out)
}

extract_guo(guo_list[[110]])

revenues[[10]][[1]] |> filter(X1 =="∟ Operating revenue (Turnover)") |> 
    select(X2) |> slice(1) |> unlist()
revenues[[1]][[1]] |>  select(last_col()) |> slice(1) |> unlist()

extract_revenues <- function(x){
    
    if(is.null(x) || x == "No data available for this company" )
        return(out <- tibble(
            last_year = NA,
            oldest_year = NA,
            revenue_last = NA,
            employees = NA,
            total_assets = NA))
    
    else
        latest_year <- x[[1]]$X2[1]
    
    oldest_year <- x[[1]] |> select(last_col()) |> slice(1) |> unlist()
    
    revenue_last <- x[[1]] |>  filter(X1 =="∟ Operating revenue (Turnover)") |> 
        select(X2) |> slice(1) |> unlist()
    employees <- x[[1]] |>  filter(X1 =="∟ Number of employees") |> 
        select(X2) |> slice(1) |> unlist()
    total_assets <- x[[1]] |>  filter(X1 == "∟ Total assets") |> 
        select(X2) |> slice(1) |> unlist()
    
    out <- tibble(
        latest_year = ifelse(is_empty(latest_year), NA, latest_year),
        oldest_year = ifelse(is_empty(oldest_year), NA, oldest_year),
        revenue_last = ifelse(is_empty(revenue_last), NA, revenue_last),
        employees = ifelse(is_empty(employees), NA, employees),
        total_assets = ifelse(is_empty(total_assets), NA, total_assets)
    )
    return(out)
}

extract_revenues(revenues[[17]])

shr_list[[1]][[1]] |> 
    select(-X1, -X3) |> 
    rename(name = X2, country = X4, type = X5, owenership_direct = X6, ownership_total = X7,
           info_source = X8, info_date = X9, operational_revenue = X10, employees_no = X11) |> 
    slice(-1,-2)  |> nrow()
    


extract_shareholders <- function(x){
    
    if(is.null(x) || x == "There is no shareholder information" )
        return(out <- tibble(
            name = NA, country = NA, type = NA, owenership_direct = NA, ownership_total = NA,
            info_source = NA, info_date = NA, operational_revenue = NA, employees_no = NA))
    
    out <- x[[1]] |> 
        select(-X1, -X3) |> 
        rename(name = X2, country = X4, type = X5, owenership_direct = X6, ownership_total = X7,
               info_source = X8, info_date = X9, operational_revenue = X10, employees_no = X11) |> 
        slice(-1,-2) 
    
    l <- nrow(out)
    
    out <- out |> slice(-l) # drop last row, it only counts number of rows.
    
    return(out)
    
}


extract_shareholders(shr_list[[1]])


#### Apply functions ####
## recover dfs

tic()
guos <- map(guo_list, extract_guo)
revs <- map(revenues, extract_revenues)
shrs <- map(shr_list, extract_shareholders)
toc() #25s

guos <- bind_rows(guos)
revs <- bind_rows(revs)


guos$company <- owners[1:2181] # the last two are missing for now
revs$company <- owners[1:2181]

shrs <- map2(shrs, owners[1:2181], function(x,y) {x$company <- y; return(x)})
shrs <- bind_rows(shrs)

shrs |> filter(!is.na(name)) |> 
    pull(company) |> 
    unique()

## there are over 1000 companies for which Orbis data is lacking. 
shrs |> filter(is.na(name)) # 1849

revs |> filter(is.na(revenue_last)) # 1538

guos |> filter(is.na(orbis_id)) # 1189

## save the data for now and try to recover more from Orbis later with advise from 
## the team


save(shrs, revs, owners, guos, file = "data/cleaned_data_230708.Rda")

## some initial visualizations
countries <- readr::read_csv(
    file = "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGCountry.csv") |> 
    janitor::clean_names()
## correct Namibia
countries[countries$short_name == "Namibia","x2_alpha_code"] <- "NA"

# most boats are fishing boats
df_boats |> 
    ggplot(aes(lli_vessel_type)) +
    geom_bar() +
    coord_flip()

# China is the largest registered fishery
a <- df_boats |> 
    group_by(flag) |> 
    summarize(n = n()) |> 
    arrange(n) |> 
    filter(n> 300) |> 
    mutate(flag = case_when(is.na(flag) ~ "Unknown", TRUE ~ flag)) |> 
    mutate(flag = as_factor(flag)) |> 
    ggplot(aes(n, flag)) +
    geom_col() +
    labs(x = "Number of vessels", y = "Flag", 
         caption = "Only countries with > 300 vessels shown.\n Data source: Lloyds",
         tag = "a") +
    theme_light(base_size = 8)

df_boats

guos |> 
    group_by(guo) |> 
    summarize(n = n()) |> 
    arrange(desc(n)) |> 
    print(n=100)

# shareholder by country
b <- shrs |> 
    filter(!is.na(name)) |> 
    mutate(country = case_when(is.na(country) ~ "NA", TRUE ~ country)) |> 
    group_by(country) |>     
    summarize(n=n()) |> 
    filter(n>10) |> 
    left_join(select(countries, short_name, country = x2_alpha_code)) |> 
    mutate(short_name = case_when(is.na(short_name) ~ "Unknown", TRUE ~ short_name)) |> 
    arrange(n) |> 
    mutate(short_name = as_factor(short_name)) |> 
    ggplot(aes(n, short_name)) +
    geom_col() +
    labs(x = "Shareholders", y = "Country of origin", caption = "Data source: Orbis",
         tag = "b") +
    theme_light(base_size = 8)

# shareholder types
c <- shrs |> #pull(type) |> unique()
    filter(!is.na(name)) |> 
    mutate(shr_type = case_when(
        is.na(type) ~ "Missing",
        type == "A" ~ "Insurance company",
        type == "B" ~ "Bank",
        type == "C" ~ "Corporate companies",
        type == "D" ~ "Unnamed private shareholders",
        type == "F" ~ "Financial company",
        type == "G" ~ "Wholesale and retail trade",
        type == "H" ~ "Self ownership",
        type == "I" ~ "Individuals or families",
        type == "J" ~ "Foundation or Research Institute",
        type == "Z" ~ "Public",
        type == "L" ~ "Employees/Managers/Directors",
        type == "M" ~ "Professional, scientific and technical activities",
        type == "V" ~ "Venture capital",
        type == "Y" ~ "Hedge fund",
        type == "P" ~ "Private equity firms",
        type == "Q" ~ "Branch",
        type == "W" ~ "Marine vessel",
        type == "S" ~ "Public authorities, States,\n Governments",
        type == "E" ~ "Mutual & Pension Fund"
    )) |> 
    group_by(shr_type) |> 
    summarize(n=n()) |> 
    arrange(n) |> 
    mutate(shr_type = as_factor(shr_type)) |> 
    ggplot(aes(n, shr_type)) +
    geom_col() +
    labs(x = "Number of shareholders", y="Shareholder type", tag = "c",
         caption = "Data source: Orbis") +
    theme_light(base_size = 8)

a + b + c & theme_light(base_size = 8)

ggsave(
    filename = "poster_descriptive_stats.png", plot = a +b +c,
    device = "png", path = "figures/", width = 7.5, height = 3,
    dpi = 500, bg = "white"
)

### networks
# 2 networks: country shareholder, country vesel flag
# shareholder companies
shrs |> pull(ownership_total) |> unique()

# correct ownerships:
shrs <- shrs |> 
    rename(ownership_direct = owenership_direct) |> 
    mutate(ownership_direct = case_when(
        ownership_direct == "WO" ~ "100",
        ownership_direct == "" ~ NA,
        ownership_direct == "MO" ~ "50",
        ownership_direct == "T" ~ "100",
        ownership_direct == "-" ~ NA,
        TRUE ~ ownership_direct
    )) |> #pull(ownership_direct) |> unique()
    mutate(ownership_total = case_when(
        ownership_total == "n.a." ~ ownership_direct,
        ownership_total == "WO" ~ "100",
        ownership_total == "MO" ~ "50",
        ownership_total == "CTP" ~ ownership_direct,
        TRUE ~ ownership_total
    )) |> #pull(ownership_total) |> unique()
    mutate(ownership_direct = str_remove_all(ownership_direct, "\\>|\\<"),
           ownership_total = str_remove_all(ownership_total, "\\>|\\<")) |> 
    #pull(ownership_total) |> unique()
    mutate(ownership_direct = as.numeric(ownership_direct),
           ownership_total = as.numeric(ownership_total))

library(network)

bip <- shrs |> 
    select(name, company, ownership_total) |> 
    filter(!is.na(name)) |> 
    slice(-338) |>  # problems: the name is the IMO and the shareholder name the same
    network(directed = FALSE, bipartite = TRUE, matrix.type = "edgelist", 
            ignore.eval=FALSE, loops = TRUE)

plot(bip)

a <- shrs |> 
    ggplot(aes(ownership_total)) +
    geom_density() +
    labs(x = "Total ownership (%)", tag = "a") +
    #scale_x_continuous(labels = scales::label_percent()) +
    theme_light()

b <- shrs |> 
    group_by(company) |> 
    summarize(n = n()) |> 
    ggplot(aes(n)) + 
    geom_bar() +
    scale_y_log10() +
    labs(x = 'Number of shareholders', y = "Companies", tag = "b") +
    theme_light()

mat <- bip |> as.sociomatrix() #|> dim()

one <- mat %*% t(mat)
shr_net <- network(one, directed = FALSE)
shr_net %e% "w" <- one
shr_net %v% "outdegree" <- sna::degree(shr_net, gmode = "digraph", cmode = "outdegree")

plot(shr_net)

library(ggnetwork)

c <- ggplot(ggnetwork::ggnetwork(shr_net, arrow.gap = 0.005) ,
       aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(aes(color = w), size = 0.15, alpha = .5
               #arrow = arrow(length = unit(2, "pt"), type = "closed")
               ) +
    geom_nodes( color = "orange", alpha = 0.4) +
    #geom_nodes(aes(size = betw), color = "blue", alpha = 0.4) +
    #geom_nodetext(aes(label = vertex.names, size = degree/4)) +
    # scico::scale_color_scico(
    #     "Channel", palette = "batlow", direction = -1,
    #     guide = guide_colorbar(
    #         barwidth = unit(3, "mm"), barheight = unit(1.5, "cm"))) +
    scale_size(name = "Outdegree", range = c(0.2,4)) +
    labs(title = "Network of shareholders",
         subtitle = "Shareholders are connected if they have shares on the same fishing companies", 
         caption = "Data source: Orbis") + 
    scale_color_continuous(
        "Companies", breaks = c(1,2,3), 
        guide = guide_colorbar(barwidth = unit(2,"cm"), barheight = unit(2, "mm")))+
    theme_blank(base_size = 6) +
    theme(legend.position = "bottom")


net <- shrs |> 
    filter(!is.na(name), !is.na(country)) |> 
    select(country, name, company) |> 
    left_join(
        df_boats |> 
            select(flag, reg_owner) |> 
            filter(!is.na(reg_owner)) |> 
            unique(),
        by = c("company" = "reg_owner")
    ) |> 
    mutate(flag = str_remove_all(flag, " \\(Int. Register\\)")) |> 
    group_by(country, flag) |> 
    summarize(n = n()) |> print(n=151)
    left_join(countries |> 
                  select(short_name, x2_alpha_code), 
              by = c("country"= "x2_alpha_code")) |> #print(n = 151)
    mutate(short_name = case_when(
        country == "TW" ~ "Taiwan, China",
        TRUE ~ short_name)) |> 
    mutate(flag = case_when(
        flag == "U.K." ~ "United Kingdom",
        flag == "Republic of Ireland" ~ "Ireland",
        flag == "U.S.A." ~ "United States",
        TRUE ~ flag),
        short_name = case_when(
            short_name == "Dem. People's Rep. Korea" ~ "South Korea",
            TRUE ~ short_name
        )) |> 
    ungroup() |> 
    select(from = short_name, to = flag, n) |>
    filter(!is.na(from)) |> #print(n = 151)
    network(directed = TRUE, bipartite = FALSE, matrix.type = "edgelist", 
            ignore.eval=FALSE, loops = TRUE)

network.vertex.names(net)[!network.vertex.names(net) %in% countries$short_name]

plot(
    net,
    edge.lwd = 0.001, edge.col = alpha("gray75", 0.75),
    vertex.col = "orange", vertex.border = 0, vertex.cex = 2)

df_stats <- tibble(
    country = network.vertex.names(net),
    indegree = sna::degree(net, gmode = "digraph", cmode = "indegree"),
    outdegree = sna::degree(net, gmode = "digraph", cmode = "outdegree")
)

df_stats <- df_stats |> 
    left_join(select(countries, country = short_name, iso2 = x2_alpha_code))



# library(gplots)
# heatmap.2(
#     as.sociomatrix(net, "n") |> log1p(),
#     trace = 'none'
#     )

w <- as.sociomatrix(net, "n") 
diag(w) <- 0

net %e% "w" <- w
net %v% "loops" <- diag(as.sociomatrix(net, "n") )
net %v% "label" <-  df_stats |> pull(iso2)
library(scico)


d <- ggplot(ggnetwork::ggnetwork(net, arrow.gap = 0.005) ,
       aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(aes(color = w), alpha = 1,
               arrow = arrow(length = unit(2, "pt"), type = "closed")) +
    geom_nodes(aes(size = loops), color = "orange", alpha = 1) +
    geom_text(aes(label = label), size = 2, color = "white") +
    scale_color_scico(
        "Shareholders",palette = "berlin", 
        guide = guide_colorbar(barwidth = unit(4,"cm"), barheight = unit(2, "mm"))) +
    scale_size(range = c(4,10), breaks = c(0, 100, 200)) +
    theme_void() + labs(tag = "c") +
    theme(legend.position = "bottom")


df_stats$loops <- net %v% "loops"

df_stats |> 
    mutate(label = case_when(
        indegree > 5 | outdegree > 5 ~ country
    )) |> 
    ggplot(aes(indegree, outdegree)) +
    geom_point(aes(size = loops), alpha = 0.5) +
    geom_abline(slope = 1, color = "orange", alpha = 0.5) +
    geom_text(aes(label = label), nudge_y = -0.5) +
    scale_size(range = c(1,10), breaks = c(0, 100, 200)) +
    theme_light()


((a/b/c) | d) + plot_layout(widths = c(1,3))


ggsave(
    filename = "poster_network.png", plot = ((a/b) | d) + plot_layout(widths = c(1,3)),
    device = "png", path = "figures/", width = 7.5, height = 5,
    dpi = 500, bg = "white"
)


#### ergms ####
library(ergm)

net2 <- network(w)

## remove nodes without stats
## load the wgi
load("data/governance_index_cleaned.RData")
df_wgi <- df_wgi |> 
    mutate(country_name = case_when(
        country_name == "Korea, Dem. People's Rep." ~ "South Korea",
        country_name == "Korea, Rep." ~ "Korea",
        country_name == "Russian Federation" ~ "Russia",
        country_name == "Venezuela, RB" ~ "Venezuela",
        TRUE ~ country_name
    )) |> 
    pivot_wider(names_from = indicator_name, values_from = estimate) |> 
    filter(country_name %in% df_stats$country)

c_name <- (df_wgi |> pull(country_name) |> unique())
df_stats$country[df_stats$country %in% c_name == FALSE] # 0 means all names are compatible!
# c_name[c_name %in% nodes == FALSE]

## load inequality data:
inq <- readxl::read_xlsx(
    path = "~/Documents/Projects/DATA/WIID_World_Income_Inequality_DB/WIID_06MAY2020.xlsx")

inq <- inq %>% 
    filter(!is.na(gini_reported), year >= 1990) %>% 
    select(country, isoa2 = c2, country_code = c3, year, gini_reported) %>% 
    group_by(country, year, isoa2, country_code) %>% 
    summarize(gini_mean = mean(gini_reported, na.rm = TRUE)) |> 
    ungroup() |> group_by(country, isoa2, country_code) |> 
    summarize(gini_mean = mean(gini_mean, na.rm = TRUE))

df_stats <- df_stats |> 
    left_join(df_wgi, by = c("country" = "country_name")) |> 
    left_join(inq, by = c("iso2" = "isoa2"))

df_stats |> select(country.x, country.y, `Control of Corruption`, gini_mean) |>  print(n=76) 

## reduced dataset with full stats
df_red <- df_stats |> 
    filter(!is.na(gini_mean)) |> 
    print(n=62)

delete_countries <- which(!df_stats$country.x %in% df_red$country.x)

net2 <- delete.vertices(net2, delete_countries)

## add network attributes
net2 %v% "corruption" <- df_red$`Control of Corruption`
net2 %v% "gov_effectiveness" <- df_red$`Government Effectiveness`
net2 %v% "pol_stability" <- df_red$`Political Stability and Absence of Violence/Terrorism`
net2 %v% "regulatory_qual" <- df_red$`Regulatory Quality`
net2 %v% "rule_law" <- df_red$`Rule of Law`
net2 %v% "voice_acc" <- df_red$`Voice and Accountability`
net2 %v% "gini" <- df_red$gini_mean


#### ergm: binary ####
# null model
f0 <- ergm(net2 ~ edges)
# governance model
tic()
f1 <- ergm(
    net2 ~ edges + diff("corruption") + diff("gini") +
        diff("gov_effectiveness") +  diff("pol_stability") +
        diff("regulatory_qual") + diff("rule_law") + diff("voice_acc"))
toc()
# governance + inequality
tic()
f2 <- ergm(
    net2 ~ edges + diff("corruption") + diff("gini") +
        diff("gov_effectiveness") +  diff("pol_stability") +
        diff("regulatory_qual") + diff("rule_law") + diff("voice_acc") +
        nodeicov("corruption") + nodeicov("gov_effectiveness") +
        nodeicov("pol_stability" ) +
        nodeicov("regulatory_qual" ) + nodeicov("rule_law") +
        nodeicov("gini"))
toc()

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

fits <- list(f0, f1, f2)
fit_name <- c("null", "difference","full")

df_stats2 <- tibble(
    model = fit_name,
    case = names(fits),
    logLik = map_dbl(fits, logLik),
    AIC = map_dbl(fits, AIC)
)

#### structural models ####
#f5 <- ergm(net_countries ~ edges + triadcensus())

fits <- fits |> map(broom::tidy, exponentiate = FALSE) 
fits <- map2(fits, fit_name, function(x,y) {x$model <- y; return(x)})
#fits <- map2(fits, names(fits), function(x,y) {x$case <- y; return(x)})

fig5c <- fits |> 
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
    theme_light() +
    theme(legend.position = c(0.15,0.25))

fig5c


ggsave(
    filename = "poster_ergm.png", plot = fig5c ,
    device = "png", path = "figures/", width = 7, height = 3,
    dpi = 500, bg = "white"
)