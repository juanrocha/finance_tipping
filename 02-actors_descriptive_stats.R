## Extracting descriptive stats of financial actors
## Juan Rocha
## 210929

library(tidyverse)
library(fs)
library(here)
library(patchwork)

# The cleaning replicates code from the networks.Rmd file:
fls <- dir_ls(path = "data/Paulas_files/Financial_actors/")


dat <- map(
    fls,
    function(x) {
        x |> 
            read_delim(
                delim = ";", col_types = "cccccccccc", name_repair = "minimal",
                col_names = c("company", "shareholder", "country", "type", "direct_percent", "total_percent", "source", "date", "op_revenue", "employees"),
                skip = 1, na = c("n.d.", "-"),
                locale = locale(encoding = "latin1")) |> 
            janitor::clean_names() }
) 


dat <- dat |> 
    bind_rows() 

dat <- dat |>
    filter(company != "") |> # 2 empty rows
    mutate(
        company = str_to_title(company),
        shareholder = str_remove_all(shareholder, " via its funds"),
        #shareholder = str_to_title(shareholder),
        total_percent = str_remove_all(total_percent, pattern = ">|±|<"),
        direct_percent = str_remove_all(direct_percent, pattern = ">|±|<"),
        direct_percent = case_when(
            direct_percent == "NG" ~ "0.01",
            direct_percent == "MO" ~ "50",
            direct_percent == "FME" ~ "NA",
            TRUE ~ direct_percent
        ), 
        total_percent = case_when(
            str_detect(total_percent,"84.94") ~ "84.94", # correct the encoding
            TRUE ~ total_percent
        ),
        op_revenue = str_replace_all(op_revenue, ",", ".")
    ) |> #pull(direct_percent) |> unique() |> as.numeric()
    mutate(
        total_percent = as.numeric(total_percent),
        direct_percent = as.numeric(direct_percent), 
        date = lubridate::my(date),
        op_revenue = as.numeric(op_revenue),
        employees = as.numeric(employees)
    )
# It introduces one NA by coercion of an "NA" as a string.

skimr::skim(dat)
# dat |> 
#     ggplot(aes(total_percent)) +
#     geom_density()


## load the unique shareholders list: it was curated manually by Paula and Alice
shareholders <- read_csv2(
    file = "data/Paulas_files/003-list_shareholders.csv", 
    locale = locale(encoding = "latin1")) |> 
    janitor::clean_names() |> 
    select(shareholder:guo_final)

# shareholders <- shareholders |> 
#     mutate(across(everything(), str_to_title))

## Problem: the unique list of stakeholders has different names / spellings that 
## the original dataset. So it is not possible to join the two datasets and account
## for global ultimate owners (GUO)

raw_names <- (dat |> pull(shareholder) |> unique()) 
processed_names <- (shareholders |> pull(shareholder) |> unique())

raw_names[!raw_names %in% processed_names] |> sort()  #|> Encoding()
# there are 29 names with different names / spelling / econding
processed_names[!processed_names %in% raw_names] |> sort() #|> Encoding()
# there are 53 names with different spellings, but some of them are
# individual persons. Since we are only interested on public companies,
# Paula did the fix manually on her file for the problematic 29.

dat <- dat |> 
    left_join(shareholders) 

dat |> filter(is.na(guo_final)) |> pull(shareholder) |> unique()

dat <- dat |> mutate(
    guo_final = case_when(
        shareholder == "PGGM CO\u0085PERATIE U.A."  ~ "PGGM COOPERATIE U.A.", 
        shareholder == "AHLSTR\u0085M CAPITAL OY" ~ "AHLSTROM CAPITAL GROUP",
        shareholder == "KYLM\u0080L\u0080 KIM" ~ "MR KIM KYLMALA",
        shareholder == "VALTIONEUVOSTON KANSLIA" ~ "VALTIONEUVOSTON KANSLIA",
        shareholder == "KANTON Z\u0086RICH"  ~ "KANTON ZURICH",
        shareholder == "BANQUE CANTONALE DE GENéVE" ~ "BANQUE CANTONALE DE GENEVE",
        shareholder == "CIMSEC NOMINEES (TEMPATAN) SDN BHD CIMB BANK FOR DATOÕ CHE LODIN BIN WOK KAMARUDDIN (MM0197)" ~ "CIMB GROUP",
        shareholder == "MERPAS CO. S.Ë R.L." ~ "MERPAS (UK) LIMITED",
        shareholder == "SOCI\u0083T\u0083 G\u0083N\u0083RALE"  ~ "SOCIETE GENERALE",
        shareholder == "QATAR HOLDING LUXEMBOURG II S.ËR.L."  ~ "GOVERNMENT OF QATAR",
        shareholder == "CAJA ESPA\u0084A DE INVERSIONES SALAMANCA Y SORIA CAJA DE AHORROS Y MONTE DE PIEDAD"  ~ "CAJA ESPANA DE INVERSIONES SALAMANCA Y SORIA CAJA DE AHORROS Y MONTE DE PIEDAD",
        shareholder == "DGIC LUXEMBOURG S.Ë R.L."  ~ "GOVERNMENT OF QATAR",
        shareholder == "CIMSEC NOMINEES (TEMPATAN) SDN BHD CIMB BANK FOR DATOÕ CHONG KAN HIUNG"  ~ "CIMB GROUP",
        shareholder == "MAYBANK NOMINEES (TEMPATAN) SDN BHD PLEDGED SECURITIES ACCOUNT FOR DATOÕ CHONG KAN HIUNG" ~ "MALAYAN BANKING BERHAD",
        shareholder == "B & C HOLDING \u0085STERREICH GMBH" ~ "B & C PRIVATSTIFTUNG",
        shareholder == "RAIFFEISENBANKENGRUPPE O\u0085 VERBUND EGEN" ~ "RAIFFEISENBANKENGRUPPE OOE VERBUND EGEN",
        shareholder == "MEDIOBANCA - BANCA DI CREDITO FINANZIARIO SOCIETË PER AZIONI" ~ "MEDIOBANCA - BANCA DI CREDITO FINANZIARIO SOCIETA PER AZIONI",
        shareholder == "DB (MALAYSIA) NOMINEES (ASING) SDN BHD BNYM SA/NV FOR PEOPLEÕS BANK OF CHINA (SICL ASIA EM)"  ~ "DB (MALAYSIA) NOMINEES (ASING) SDN BHD BNYM SA/NV FOR PEOPLES BANK OF CHINA (SICL ASIA EM)",
        shareholder == "KANSANEL\u0080KELAITOS" ~ "KANSANELAKELAITOS",
        shareholder == "VALTION EL\u0080KERAHASTO"  ~ "SUOMEN VALTIO/ALV",
        shareholder == "SVENSKA LITTERATURS\u0080LLSKAPET I FINLAND RF"  ~ "SWEDISH LITERATURE SOCIETY IN FINLAND RF",
        shareholder == "SELIGSON & CO RAHASTOYHTI\u0085 OYJ" ~ "SELIGSON & CO RAHASTOYHTIO OYJ",
        shareholder == "S\u0080\u0080ST\u0085PANKKI KOTIMAA -SIJOITUSRAHASTO" ~ "SAASTOPANKKI KOTIMAA -SIJOITUSRAHASTO",
        shareholder == "DIRE\u0082ÌO-GERAL DO TESOURO E FINAN\u0082AS"  ~ "DIRECCAO-GERAL DO TESOURO E FINANCAS",
        shareholder == "HERAN\u0082A INDIVISA DE MARIA RITA DE CARVALHOSA MENDES DE ALMEIDA DE QUEIROZ PEREIRA"  ~ "MR HERANCA INDIVISA DE MARIA RITA DE CARVALHOSA MENDES DE ALMEIDA DE QUEIROZ PEREIRA",
        shareholder == "KYMIN OSAKEYHTI\u0085N 100-VUOTISS\u0080\u0080TI\u0085 SR"   ~ "KYMIN OSAKEYHTION 100-VUOTISSAATIO SR",
        shareholder == "SIGRID JUS\u0083LIUS STIFTELSE SR"  ~ "SIGRID JUSELIUS STIFTELSE SR",
        shareholder == "STIFTELSEN F\u0085R \u0081BO AKADEMI SR"  ~ "STIFTELSEN FOR AABO AKADEMI SR",
        shareholder == "F\u0085RENINGEN KONSTSAMFUNDET R.F." ~ "FORENINGEN KONSTSAMFUNDET R.F.",
        TRUE ~ guo_final
    )
) 

dat |> filter(is.na(guo_orbis)) |> select(-c(source:employees))
dat |> filter(is.na(guo_final)) |> select(-c(source:employees))
#### J211010: This dataset still has some names that correspond to private persons, 
#### not public companies. There were errors on Paula's list of GUOs, I corrected 
#### them manually with the look up table: 003-list_shareholders_v2

dat |> 
    filter(!is.na(direct_percent), !is.na(total_percent)) # 18 obs

dat |> 
    filter(is.na(direct_percent), is.na(total_percent)) # 302 obs

## there are 34 cases where shareholders and companies are reported more than once
## the reason is multiple sources (see Orbis help under information providers)
## but a reasonable approach is to keep the most recent record only; or the max
## if reported on the same date
dat |> 
    group_by(company, shareholder) |> 
    add_count() |> 
    filter(n > 1) |> 
    arrange(desc(shareholder)) |> 
    print(n = 45) 
    

dat <- dat |> 
    mutate(shareholder = str_to_title(shareholder),
           guo_orbis = str_to_title(guo_orbis),
           guo_final = str_to_title(guo_final))|> 
    group_by(company, shareholder) |> 
    add_count(name = "count") |> 
    mutate(ownership = pmax(direct_percent, total_percent, na.rm = TRUE),
           is_max = (ownership == max(ownership, na.rm = TRUE)))

dat |> filter(count >1) |>arrange(desc(shareholder)) |> print(n = 45)
dat |> filter(count == 1, is.na(is_max)) # there is 298 records with no ownership (NA in direct and total %)

dat <- dat |> 
    filter(is_max == TRUE) # reduces the dataset to 5683obs

## Still errors: why? the final guo is also reported as shareholder, so double counted
dat |> 
    ungroup() |> 
    group_by(company, guo_final) |> 
    summarize(total_own = sum(ownership)) |> 
    arrange(desc(total_own)) |> 
    print(n=10)

## the two problematic cases:
dat |> filter(company == "Bio Pappel S.a.b") ## It seems that it double counts when accounting for individual persons
dat |> filter(company == "Th Plantations Berhad") |> #print(n=32)
    filter(direct_percent> 70 | total_percent > 70) |> 
    select(shareholder) ## It seem32)here was a big move of shares ~70% from one owner to another. Since we don't know who sold, we only keep the most recent data for this case.

## Solution: filter out the strange cases
dat <- dat |> 
    ungroup() |> 
    filter(company != "Bio Pappel S.a.b" | type != "I") |> # get's rid of the individual
    # Lembaga Tabung Haji is a bank that owned >70% of the company that a year later is owned on similar % by the Government of Malasya. It get rid of two obs
    filter(company != "Th Plantations Berhad" | shareholder != "Lembaga Tabung Haji") 
## Reduces the data set to 3825 observations
## And the test is passed
dat |> 
    ungroup() |> 
    group_by(company, guo_final) |> 
    summarize(total_own = sum(ownership)) |> 
    arrange(desc(total_own)) ## Nothing above 100%

## So there is still double values when the guo_final is duplicated, but that is because the 
## ownership is through different intermediaries (shareholders in the group_by).
dat |> 
    select(-count, -is_max, -op_revenue, -employees, -type, -source) |> 
    ungroup() |> 
    group_by(company, shareholder, guo_final) |> 
    add_count(name = "count") |> 
    filter(count > 1) 

## remove duplicated entries:
dat <- dat |> unique()

## remove individual persons from the analysis
dat <- dat |> filter(type != "I")

# save(dat, file = "data/investors_cleaned.RData")

### Visualizations
load("data/investors_cleaned.RData")

dat <- dat |>
    mutate(holdings = ownership * 0.05)

dat |> 
    ggplot(aes(holdings)) + 
    geom_density() +
    scale_x_log10()

g1 <- dat |> 
    group_by(guo_final) |> 
    summarise(companies = n()) |> 
    arrange(desc(companies)) |> 
    top_n(25) |> 
    mutate(guo_final = as_factor(guo_final) |> 
               fct_reorder( companies, sort)) |> 
    ggplot(aes(companies, guo_final)) +
    geom_col() +
    labs(title = "", x = "Number of companies", y ="Global Unique Owner") +
    theme_light(base_size = 8)

g2 <- dat |> 
    group_by(guo_final) |> 
    summarise(holdings = sum(holdings)) |>  
    arrange(desc(holdings)) |> 
    top_n(25) |> 
    mutate(guo_final = as_factor(guo_final) |> 
               fct_reorder( holdings, sort)) |> 
    ggplot(aes(holdings, guo_final)) +
    geom_col() +
    labs(title = "", x = "Number of holdings", y ="")+
    theme_light(base_size = 8)

# g3 <- df3 |> 
#     arrange(desc(size_ownership)) |> 
#     top_n(10) |> 
#     mutate(guo_final = as_factor(guo_final) |> 
#                fct_reorder( size_ownership, sort)) |> 
#     ggplot(aes(size_ownership, guo_final)) +
#     geom_col() +
#     labs(title = "", x = "Millions of dollars", y ="")+
#     theme_light(base_size = 7)

g1 + g2 

ggsave(
    filename = "top_shareholders.png",
    plot = (g1 +g2),
    device = "png",
    path = "figures/",
    width = 6, height = 4,
    bg = "white", dpi = 400
)
 