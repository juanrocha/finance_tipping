## Title: Financial Actors Analysis
## Author: Paula A. Sanchez
## Modified by Juan
## date: 4/26/2021 and 20210928


library(tidyverse)


companies  <- read.csv("data/Paulas_files/002-PublicCompanies.csv", header = TRUE, sep = ";")
|> unique.shareholders <- read.csv("data/Paulas_files/003-list_shareholders_v2.csv", header = TRUE, sep = ";", fileEncoding = "latin1")

companies <- companies %>%  filter(Type == "Public")

colnames(unique.shareholders) <- c("Shareholder", "GUO.orbis", "GUO.final")
unique.shareholders <- unique.shareholders %>% select(-c(GUO.orbis))
  

names.files <- dir("data/Paulas_files/Financial_actors/")

## Create an empty vector for merging all files 

shareholders <- data.frame(matrix(ncol=10, nrow=0))
colnames(shareholders) <- c("Company", "Shareholder", "Country", "Type", "Direct", "Total", "Source", "Date", "Revenue", "Employees")

## Using a loop to create a unique data frame containing all shareholders 

for(i in 1:length(names.files)){
  
  scrfile <- paste("data/Paulas_files/Financial_actors/", names.files[i], sep="")
  data <- read.csv(scrfile, header = TRUE, fileEncoding = "latin1", sep=";")
  
  colnames(data) <- c("Company", "Shareholder", "Country", 
                      "Type", "Direct", "Total", "Source", 
                      "Date", "Revenue", "Employees")
  data$Shareholder <- gsub(" via its funds", "", data$Shareholder)
  
  
  shareholders <- rbind(shareholders, data)
  
}

## Replacing non-numeric values in data frame and converting Direct and Total ownership into numeric classes

shareholders$Direct <- gsub("±", "", shareholders$Direct)
shareholders$Direct <- gsub("<", "", shareholders$Direct)
shareholders$Direct <- gsub(">", "", shareholders$Direct)

shareholders$Total <- gsub("±", "", shareholders$Total)
shareholders$Total <- gsub("<", "", shareholders$Total)
shareholders$Total <- gsub(">", "", shareholders$Total)

## J210928: This needs explanation: What is FME, NG, MO?
shareholders$Direct[shareholders$Direct =="-" | shareholders$Direct == "n.d." | 
                      shareholders$Direct == "FME" | shareholders$Direct == ""] <- "-99999"
shareholders$Direct[shareholders$Direct =="NG"] <- "0.01"
shareholders$Direct[shareholders$Direct =="MO"] <- "50.00"

shareholders$Total[shareholders$Total =="-" | shareholders$Total == "n.d." | 
                     shareholders$Total == "FME" | shareholders$Total == ""] <- "-99999"
shareholders$Total[shareholders$Total =="NG"] <- "0.01"
shareholders$Total[shareholders$Total =="MO"] <- "50.00"

shareholders <- shareholders %>% 
    mutate(Direct = as.numeric(Direct), 
           Total = as.numeric(Total)) |> 
    as_tibble()

## Up to here it is similar to my code, script networks.Rmd, chunk financial-actors

## Assigning the GUO information (WARNING: 47 SHAREHOLDERS ARE NOT FOUND IN THE ORIGIN DATA FRAME)

shareholders <- shareholders %>% 
    left_join(unique.shareholders) %>%
    select(GUO.final, Shareholder, Company, Country:Employees)

## Replacing the Shareholders that due to an encoding problem could not be merged from the 003-list_shareholders.csv

missing.shareholders <- shareholders %>% filter(is.na(GUO.final))

df.missing <- data.frame(matrix(ncol = 2, nrow = 29))
colnames(df.missing) <- c("Shareholder", "GUO.final")

df.missing$Shareholder <- unique(missing.shareholders$Shareholder)
df.missing$GUO.final <- c("PGGM COOPERATIE U.A.", 
                          "AHLSTROM CAPITAL-KONSERNI, AC INVEST FIVE B.V.",
                          "MR KIM KYLMALA",
                          "VALTIONEUVOSTON KANSLIA",
                          "KANTON ZURICH",
                          "BANQUE CANTONALE DE GENEVE",
                          "CIMSEC NOMINEES (TEMPATAN) SDN BHD CIMB BANK FOR DATO√ï CHE LODIN BIN WOK KAMARUDDIN (MM0197)",
                          "MERPAS (UK) LIMITED",
                          "SOCIETE GENERALE",
                          "QATAR HOLDING LUXEMBOURG II S.ËR.L.",
                          "CAJA ESPANA DE INVERSIONES SALAMANCA Y SORIA CAJA DE AHORROS Y MONTE DE PIEDAD",
                          "GOVERNMENT OF QATAR",
                          "CIMSEC NOMINEES (TEMPATAN) SDN BHD CIMB BANK FOR DATO√ï CHONG KAN HIUNG",
                          "MAYBANK NOMINEES (TEMPATAN) SDN BHD PLEDGED SECURITIES ACCOUNT FOR DATO√ï CHONG KAN HIUNG",
                          "B & C PRIVATSTIFTUNG",
                          "RAIFFEISENBANKENGRUPPE OOE VERBUND EGEN",
                          "MEDIOBANCA - BANCA DI CREDITO FINANZIARIO SOCIETA PER AZIONI",
                          "DB (MALAYSIA) NOMINEES (ASING) SDN BHD BNYM SA/NV FOR PEOPLE√ïS BANK OF CHINA (SICL ASIA EM)",
                          "KANSANELAKELAITOS",
                          "SUOMEN VALTIO/ALV",
                          "SWEDISH LITERATURE SOCIETY IN FINLAND RF",
                          "SELIGSON & CO RAHASTOYHTIO OYJ",
                          "SAASTOPANKKI KOTIMAA -SIJOITUSRAHASTO",
                          "DIRECCAO-GERAL DO TESOURO E FINANCAS",
                          "MR HERANCA INDIVISA DE MARIA RITA DE CARVALHOSA MENDES DE ALMEIDA DE QUEIROZ PEREIRA",
                          "KYMIN OSAKEYHTION 100-VUOTISSAATIO SR",
                          "SIGRID JUSELIUS STIFTELSE SR",
                          "STIFTELSEN FOR AABO AKADEMI SR",
                          "FORENINGEN KONSTSAMFUNDET R.F.")

df1.shareholders <- shareholders %>% inner_join(df.missing, by = "Shareholder") %>%
  mutate(GUO.final = coalesce(GUO.final.x, GUO.final.y)) %>%
  select(GUO.final, Shareholder, Company, Country:Employees) 

df2.shareholders <- shareholders %>% filter(!is.na(GUO.final))

shareholders <- rbind(df1.shareholders, df2.shareholders)

## Calculate the percentage of ownership according to Alice's documentation for duplicates within a company

ownership.duplicates <- shareholders %>% 
  group_by(Company) %>% 
  filter(duplicated(GUO.final)) %>% # show unique rows that are duplicated, but does not pull out all of them
  ungroup() %>%
  select(Company, GUO.final, Direct, Total) %>% 
    ## this line is wrong, it converts to character and separates on minus sign or decimal point
  separate(Direct, c("direct.1", "direct.2"), extra = "merge") %>%
    ## this line is also wrong, it converts to character and separates on minus sign or decimal point
  separate(Total, c("total.1", "total.2"), extra = "merge") %>%
    # this recover the missing values coded as -99999
  mutate(direct.1 = replace(direct.1, direct.1 == "", -99999),   
         total.1 = replace(total.1, total.1 == "", -99999),
         direct.2 = replace(direct.2, direct.2 == "", -99999),   
         total.2 = replace(total.2, total.2 == "", -99999),
         direct.1 = replace(direct.1, direct.1 == "99999", -99999),   
         total.1 = replace(total.1, total.1 == "99999", -99999),
         direct.2 = replace(direct.2, direct.2 == "99999", -99999),   
         total.2 = replace(total.2, total.2 == "99999", -99999)) %>%
    # this make them numeric again given that they were transformed to strings when separated
  mutate(direct.1 = as.numeric(direct.1), direct.2 = as.numeric(direct.2),
          total.1 = as.numeric(total.1), total.2 = as.numeric(total.2)) %>% 
  replace(is.na(.), -99999) %>%
    # this is wrong: you are summing integers with decimals!
  mutate(Ownership = 
           case_when(total.1 == -99999 & total.2 == -99999 & 
                       direct.1 != -99999 & direct.2 != -99999 ~ (direct.1+direct.2),
                     total.1 == -99999 & total.2 == -99999 & 
                       direct.1 == -99999 & direct.2 == -99999 ~ -99999,
                     TRUE ~ pmax(total.1, total.2, direct.1, direct.2))) %>%
  select(GUO.final, Company, Ownership)

## This chunk of code is wrong. I believe the intent was to add the different direct and indirect holdings
## per company into the GUO.final. It errs at the beginning by finding the duplicated rows, 
## but not pulling out all of them. Then it separates the holdings into integers and decimal units 
## **for the same observation**. 
## A test that the table is wrong is summing ownerships, it should never be > 100%

ownership.duplicates |> 
    group_by(GUO.final) |> 
    mutate(Ownership = ifelse(Ownership == -99999, NA, Ownership)) |> 
    summarise(sum = sum(Ownership, na.rm = TRUE)) |> 
    arrange(desc(sum))



## Percentage of ownership (direct or total) for cases without duplicates

no.duplicates <- shareholders %>% 
  group_by(Company) %>% 
  filter(!duplicated(GUO.final)) %>%
  ungroup() %>%
    ## I think it is easier to declare NAs explicitely and use the rm.na option in the pmax
  mutate(Ownership = 
           case_when(Total != -99999 & Direct == -99999 ~  Total,
                     Total == -99999 & Direct != -99999 ~ Direct,
                     Total == -99999 & Direct == -99999 ~ -99999,
                     Total != -99999 & Direct != -99999 ~ pmax(Total, Direct)))
 

duplicates <- shareholders %>% 
  group_by(Company) %>% 
  filter(duplicated(GUO.final)) %>%
  ungroup() %>% 
  distinct(GUO.final, Company, .keep_all = TRUE)%>%
  left_join(ownership.duplicates)


shareholders.ownership <- rbind(no.duplicates, duplicates)

## test again: Fail -- there are GUO with more than 100% of a company
shareholders.ownership |> 
    group_by(GUO.final, Company) |> 
    mutate(Ownership = ifelse(Ownership == -99999, NA, Ownership)) |> 
    summarise(sum = sum(Ownership, na.rm = TRUE)) |> 
    arrange(desc(sum))


## Determining the number of sectors and market cap for each company

public.companies <- data.frame(matrix(ncol = 4, nrow = 96))
colnames(public.companies) <- c("Company",	"Region",	"Commodity",	"Market.cap.mll")

public.companies$Company <- unique(companies$Company)

public.companies$Region <- companies %>% 
  select(Company, Region) %>% 
  group_by(Company) %>% 
  tally()  %>% 
  ungroup()  %>% 
  pull(-Company)

public.companies$Commodity <- companies %>% 
  select(Company, Commodity) %>% 
  group_by(Company) %>% 
  tally()  %>% 
  ungroup()  %>% 
    ## this does not guarantee that companies are on the same order, so you might get errors here
  pull(-Company)

public.companies$Market.cap.mll <- companies %>% 
  select(Company, Market.cap.mll) %>% 
  group_by(Company) %>% 
    ## Not sure this is correct, you are averaging across commodities: check below
  summarise(Market.cap.mll = mean(Market.cap.mll))  %>% 
  ungroup()  %>% 
  pull(-Company)

## test: companies have unique market caps
companies |> as_tibble() |> 
    select(Company, Market.cap.mll) |> #130 rows
    unique() |>  # 96 rows
    pull(Company) |> unique() |> length() # 96 companies. Mean work because there is only one value!


shareholders.ownership <- shareholders.ownership %>% left_join(public.companies)

## How many holdings do shareholders have? 1) Calculated the number of holding per company and then summarize


shareholders.ownership <- shareholders.ownership %>% 
    mutate(Holdings = 
               if_else(Ownership == -99999, 
                       -99999,
                       floor(Ownership * 0.2)), 
# where the numbers come from?: I think it's chuncks of 5% holdings so ownership / 5 = ownership *0.2
# but unclear with size ownership
           Size.Ownership = Ownership*Market.cap.mll*.1) 

summary <- shareholders.ownership %>% 
  filter(Holdings > 0, !is.na(Size.Ownership))  %>% 
  group_by(GUO.final) %>%  
  summarise(Companies = n(), 
            Holdings = sum(Holdings),
            Region = sum(Region),
            Commodity = sum(Commodity),
            Size.Ownership = sum(Size.Ownership)) %>% 
  arrange(desc(Holdings), desc(Companies)) %>%
    ## Why do you foce max values to 6 and 5? One get higher values than the originally coded for because of the
    ## duplicates and because a company can be on the same region with more than one commodity and more than one company
    ## But forcing to 5 or 6 is incorrect because it can be only 2 regions with plenty of companies in each.
  mutate(Region = replace(Region, Region > 6, 6),
         Commodity = replace(Commodity, Commodity > 5, 5))
  
## Juan: re-do this two tables 

write.csv(summary, "004-Financial_summary.csv")
write.csv(shareholders.ownership, "005-FinancialData.csv")

