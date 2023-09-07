###### Zoonotic disease risk paper
###### Computing Herfindahl-Hirschman (HH) index of ownership concentration for companies and case studies
###### Script written by Alice Dauriach, last updated 2023-09-06

### Preparing
#install.packages("dplyr")
library("dplyr")

`%notin%` <- Negate(`%in%`)

setwd("/home/alice/Documents/GEDB/")
df <- as_tibble(read.csv("230823_cases_dataframe.csv"))


### Checking sum of ownership for each company
test1 <- aggregate(df$ownership, by=list(company=df$company, casestudy=df$casestudy), FUN=sum)

#There seems to be a serious problem with how the ownership figure is calculated. When a shareholder appears twice in Orbis, with a direct ownership figure AND a total ownership figure, our previous method was to take only the larger of these two figures. But here, instead of taking the maximum, we retain both rows with both ownership figures. The "is_max" column is TRUE at every row. This means double counting of many ownership stakes, which leads to the total ownership per company often exceeding 100%, and which leads to a severe bias which overestimates the stake of shareholders which appear twice in Orbis (with a "direct" and a "total" ownership - usually given by different sources of information to Orbis, which simply reports both figures). Please have a look in your other calculations of the ownership stakes in the network to see whether you solve this problem at a later stage.

#I also noticed that some ownership links between shareholders were missing (i.e., the shareholders are listed as having different GUOs when in fact they have the same one). I do a manual check of all the largest shareholders (visual check trying to spot shareholders with similar names or similar share percentages in the same company with different sources of information).


### Manual check of all shareholders

#Ballarpur Industries: 108% ownership but I can't find an easy-to-spot duplicate. The main owner is indirect with 49% - keeping only direct owners would lead to a very misleading HH index... 
# Graphic Packaging Holding Company: same, 108%, although there is no overwhelming shareholder. Some of the asset managers have direct and or total ownership, which may explain the discrepancy. Same for Louisiana Pacific Corp (102%). Same for Olam (102%)
# Credit Suisse Trust Limited should be considered a subsidiary of Credit Suisse Group Ag. Same with Ninety One Africa (Pty) Ltd and Ninety One Plc; same for Ichigo; Amanahraya, Employees Provident Fund Board and Govt of Malaysia (according to their website) and Yayasan Amanah Hartanah Bumiputera (according to: https://www.freemalaysiatoday.com/category/nation/2023/02/24/govt-bodies-to-face-closure-merger-and-executive-pay-review/)
#Sumitumo Mitsui Trust Holdings and Financial Group: according to Wikipedia, NO capital relation.
#Despite cleaning, Wilmar still has 108% total ownership which I can't explain.

df <- df %>% mutate(
  guo_final = case_when(
    guo_final == "Credit Suisse Trust Limited" ~ "Credit Suisse Group Ag",
    guo_final == "Ninety One Africa (Pty) Ltd" ~ "Ninety One Plc",
    guo_final == "Ichigo Foundation" ~ "Ichigo Trust",
    guo_final == "Jardine Matheson (Bermuda) Ltd." ~ "Jsh Asian Holdings Ltd",
    grepl("Amanahraya", guo_final) ~ "Government Of Malaysia",
    guo_final == "Employees Provident Fund Board" ~ "Government Of Malaysia",
    guo_final == "Yayasan Amanah Hartanah Bumiputera" ~ "Government Of Malaysia",
    guo_final == "Swedish Literature Society In Finland Rf" ~ "The Society Of Swedish Literature In Finland",
    guo_final == "Atlas Capital Resources Ii Lp" ~ "Atlas Holdings Llc",
    guo_final == "Ppb Group Berhad" ~ "Kuok Brothers Sdn Bhd",
    guo_final == "Kuok (Singapore) Limited" ~ "Kuok Brothers Sdn Bhd",
    
    TRUE ~ guo_final
  )) %>% 
  filter(shareholder != "Public") #there is this weird entry for one company which I think is supposed to capture "remaining" shares publicly traded, which we can safely remove since it does not refer to any actual shareholder and is clearly a double-count


### Correcting the ownership column
df_corrected <- df %>% 
  group_by(company, casestudy, guo_final) %>%
  summarise(direct_percent = max(direct_percent, na.rm = TRUE),
            total_percent = max(total_percent, na.rm = TRUE),#here I take the max and not the sum because the sources are different and I suspect this is why we get a lot of double-counting and total stakes exceeding 100%.
            .groups = "drop") %>% 
  mutate(
    direct_percent = case_when(
      direct_percent == -Inf ~ NA_real_,
      TRUE ~ direct_percent
    ),
    total_percent = case_when(
      total_percent == -Inf ~ NA_real_,
      TRUE ~ total_percent
    )
  ) %>% 
  rowwise() %>% 
  mutate(ownership_corrected = max(direct_percent, total_percent, na.rm = TRUE)) %>% 
  group_by(company, casestudy) %>% 
  mutate(total_ownership_corrected = sum(ownership_corrected)) %>% 
  ungroup() %>% 
  select(company, casestudy, guo_final, ownership_corrected, total_ownership_corrected, direct_percent, total_percent) %>% 
  arrange(company, casestudy, desc(ownership_corrected))


test2 <- aggregate(df_corrected$ownership_corrected, by=list(company=df_corrected$company, casestudy=df_corrected$casestudy), FUN=sum)
# it's better but there are still 5 companies with more than 100% ownership. Maximum 108.6%.
rm(test1, test2)


### Calculating HH indexes

# We exclude these 5 companies with total ownership exceeding 100% from the HHI calculations- since we know there is some problem with their list of shareholders. And computing an HHI would lead to a fundamental problem (total exceeding 10,000). We also exclude companies where we have info on less than 50% of the ownership - since we might be missing important, controlling shareholders, and the resulting HHI index just wouldn't make sense given the information scarcity. 

hist(df_corrected$total_ownership_corrected)

df_corrected %>% group_by(company) %>% 
  summarise(total_ownership_corrected = first(total_ownership_corrected)) %>% 
  filter(total_ownership_corrected >= 100 | total_ownership_corrected <= 50)
# We thus exclude 14 companies in total.
df_corrected %>% group_by(company) %>% 
  summarise(total_ownership_corrected = first(total_ownership_corrected)) %>% 
  filter(total_ownership_corrected <= 100 & total_ownership_corrected > 50)
# We are thus left with 40 companies.

df_hhi <- df_corrected %>% 
  filter(total_ownership_corrected <= 100 & total_ownership_corrected > 50) %>% 
  group_by(casestudy, company) %>% 
  mutate(sq.ownership = ownership_corrected^2,
         lowest_ownership = min(ownership_corrected),
         missing_ownership_info = 100 - total_ownership_corrected) %>% 
  mutate(unknown.owners.sq.own1 = missing_ownership_info*lowest_ownership, #"middle" upper bound
         unknown.owners.sq.own2 = missing_ownership_info^2)
# I compute three measures of the HH index:
### a theoretical lower bound: considering only the shares of known shareholders, and ignoring the shares of missing shareholders (this assumes that any remaining unknown shareholders each have an infinitely small share)
### a "reasonable assumption" upper bound (1): this assumes that Orbis gives us the top shareholders, and any remaining missing shareholders hold shares lower than the smallest shareholder reported in Orbis. Of course this is only an assumption and it might be false, Orbis might be missing larger shareholders. To compute this, we assume that the ownership not accounted for in Orbis is distributed equally between shareholders who each hold a share equal to the one held by the smallest known shareholder.
### a theoretical upper bound (2): this is an unlikely theoretical case where we assume that the ownership not accounted for in Orbis is entirely own by a single unknown shareholder. This is the absolute upper bound to the HH index.

df_hhi <- df_hhi %>% 
  summarise(hhi_lower_bound = round(sum(sq.ownership)),
            unknown.owners.sq.own1 = first(unknown.owners.sq.own1),
            unknown.owners.sq.own2 = first(unknown.owners.sq.own2),
            .groups = "drop") %>% 
  mutate(hhi_upper_bound1 = hhi_lower_bound + unknown.owners.sq.own1,
         hhi_upper_bound2 = hhi_lower_bound + unknown.owners.sq.own2) %>% 
  select(-unknown.owners.sq.own1, -unknown.owners.sq.own2)

## Output table: HH index per company
df_hhi <- df_hhi %>% 
  mutate(discrepancy_percent1 = round(100*(hhi_upper_bound1-hhi_lower_bound)/hhi_lower_bound), # I compute the discrepancy (in %) between the lower bound and the first upper bound
         discrepancy_percent2 = round(100*(hhi_upper_bound2-hhi_lower_bound)/hhi_lower_bound)) # I compute the discrepancy (in %) between the lower bound and the second upper bound
df_hhi

## Output table: average HH index per case study
df_hhi_casestudy <- df_hhi %>% 
  group_by(casestudy) %>% 
  summarise(avg_hhi_lower_bound = mean(hhi_lower_bound),
         avg_hhi_upper_bound1 = mean(hhi_upper_bound1),
         avg_hhi_upper_bound2 = mean(hhi_upper_bound2),
         nb_companies = n(),
         .groups = "drop") %>% 
  mutate(discrepancy_percent1 = round(100*(avg_hhi_upper_bound1-avg_hhi_lower_bound)/avg_hhi_lower_bound),
         discrepancy_percent2 = round(100*(avg_hhi_upper_bound2-avg_hhi_lower_bound)/avg_hhi_lower_bound))
df_hhi_casestudy

