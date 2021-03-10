library(tidyverse)
library(plotly)
library(tictoc)

#### FAO data on crops ####
file <- "~/Documents/Projects/DATA/FAOSTAT/CommodityBalances_Crops_E_All_Data_(Normalized).csv"

dat <- read_csv(file) %>% 
  janitor::clean_names()


## descriptive 
tic()
skimr::skim(data = dat)
toc() #28 s



countries <- dat %>% pull(area) %>% unique() # 216 areas of which 182 are countries
dat %>% pull(item) %>% unique() # 112 foods and other ag products (no animals)
dat %>% pull(item_code) %>% unique() # are the codes standard across UN?
dat %>% pull(element) %>% unique() # interesting, it has production (total?), import and exports, domestic supply, losses, food supply...
dat %>% pull(flag) %>% unique() # S, A, SD?
# S: standardized data
# A: Aggregate, may include official, semi-official, estimated or calculated data 
# SD: statistical discrepancy
# 


dat %>% 
  select(-element_code, -year_code, -flag) %>% 
  filter(area == "Colombia") %>% 
  filter(item == "Coffee and products") %>% 
  pivot_wider(names_from = element, values_from = value) %>%
  mutate(
    total = `Domestic supply quantity` + `Export Quantity`
  ) %>%
  pivot_longer(cols = Production:total, names_to = "element", values_to = "value") %>%
  # filter(element != "total") %>%
  #group_by(item, year) %>% 
  ggplot(aes(year, value)) +
  geom_line(aes(group = element, color = element)) +
  scale_y_continuous(labels = scales::label_scientific())

## calcualte ratios
tic()
ratios <- dat %>% 
  select(-element_code, -year_code, -item_code, -unit) %>% 
  filter(area %in% countries[1:182])
  pivot_wider(names_from = element, values_from = value) %>% 
  # avoid divisions by zero:
  filter(
    !is.na(`Export Quantity`),  
    Production != 0,
    !is.na(`Domestic supply quantity`),
    `Domestic supply quantity` != 0
  ) %>%  # reduces the dataset from 1M obs to 0.5
  group_by(year, area, item) %>% 
  mutate(
    export_ratio = `Export Quantity` / `Domestic supply quantity`,
    export_prop = `Export Quantity` / Production
  ) 
toc() ## 7 s

ratios %>% ggplot(aes(export_ratio)) + geom_density() + scale_x_log10()
ratios %>% filter(export_prop > 10)

ratios %>% 
  filter(export_prop > 10) %>% 
  select(`Export Quantity`, `Domestic supply quantity`, Production, starts_with("export"), flag) %>% 
  filter(Production > 0)

ratios %>%
  filter(export_ratio > 100) %>% 
  select(Production,`Domestic supply quantity`, export_ratio, `Export Quantity`, `Food supply quantity (tonnes)`)


ratios_summary <- ratios %>% 
  ungroup() %>% 
  group_by(area,item) %>% 
  summarize(
    export_ratio = median(export_ratio, na.rm = TRUE),
    export_prop = median(export_prop, na.rm = TRUE)
  )
# note that some values seem calculated so they fit the balance sheet but does not make sense
# for example, there are negative values on domestic supply making the export + domestic = production hold,
# but creating negative domestic consumption!
# one should drop then any obs where production == 0, or where export or domestic is > production. These are
# inconsistencies that one cannot trust.
# 
# J210223: work with export_prop < 10 

## graphs



ratios_summary %>% ungroup() %>% 
  filter(area %in% countries[1:182]) %>% 
  #filter(export_ratio < 10 , export_ratio > -10) %>% 
  group_by(area) %>% 
  mutate(area = as_factor(area)) %>% 
  summarize(x = median(export_prop)) %>% 
  arrange(desc(x))


%>% 
  ggplot(aes(item, area)) +
  geom_tile(aes(fill = export_ratio)) +
  scale_fill_gradient2(mid = "gray85", midpoint = 0) +
  theme_light(base_size = 7) +
  theme(axis.text.x = element_text(angle = 90))

### alternatively do a heatmap aggregated by mean(year)
# ratios %>% 
#   filter(area %in% countries[1:182]) %>% 
#   filter(export_ratio < 10 , export_ratio > -10) %>% 
#   ggplot(aes(year, export_ratio)) +
#   geom_line(aes(group = area), size = 0.5) 

g3 <- ratios %>% 
  filter(area == "Mexico") %>% 
  ggplot(aes(year, export_ratio)) +
  geom_line(aes(group = item)) +
  theme_light(base_size = 8) 



g1 <- dat %>%
  filter(area == "Mexico") %>%
  filter(element == "Domestic supply quantity") %>%
  ggplot(aes(year, value)) +
  geom_line(aes(group = item), show.legend = FALSE) +
  labs(
    title = "Domestic supply quantity",
    subtitle = "Unit values are on tonnes as reported by countries",
    caption = "Data: FAO stats, @juanrocha") +
  theme_light()

g2 <- dat %>%
  filter(area == "Mexico") %>%
  filter(element == "Export Quantity") %>%
  ggplot(aes(year, value)) +
  geom_line(aes(group = item), show.legend = FALSE) +
  labs(
    title = "Export Quantity",
    subtitle = "Unit values are on tonnes as reported by countries",
    caption = "Data: FAO stats, @juanrocha") +
  theme_light()

ggplotly(g3)


