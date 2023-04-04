library(tidyverse)
library(RSelenium)
library(keyring)
library(rvest)
library(tictoc)


## todo:
## - implement the search in such a way that always picks up the first company (most likely result)
## - check before going into shareholders if there are shareholders on the first result page: solved
## - If not, create an error / message and record that as response: solved | it does not create an error message but return a df saying there is no companies.
## - if multiple shareholders, loop through pages
## - Create a loop or walking function that go through all of them (safely?)
## 
## - download the sardina-anchoveta data and tuna; for tuna use the open ocean project

dat <- read_csv(file = "data/fishing_companies_alaskan-pollock.csv") # pollock
dat <- read_csv(file = "data/fishing_comps_sardina-anchoveta.csv") # sardine-anchovies
load("data/shr_class.RData")

## Bolivian companies
dat <- readxl::read_xlsx(path = "data/bolivian-soy-which-are-the-biggest-exporting-companies-in-2020.xlsx", sheet = 1) |> 
    rename(company = exporter_group)
dat <- dat |> 
    # very useful reprex: all caracters from the begining until one finds -i
    mutate(company = str_remove(company, pattern = "^.*?(?=- )") |> 
               str_remove("- "))


## Initialize remote driver
d <- rsDriver(
    port = 4445L, browser = "firefox", extraCapabilities = list(acceptInsecureCerts = TRUE)) # should open a chrome
remDr <- d[["client"]]
#remDr$open()
tic()
remDr$navigate(url = "https://ezp.sub.su.se/login?url=https://orbis4.bvdinfo.com/ip")
toc() # >40s
## Note: working with VPN skips authentification steps! but takes longer to load
Sys.sleep(30)

## Authentication
# me <- remDr$findElement("id", "username")
# # me$clearElement()
# me$sendKeysToElement(list("juro7132"))
# pswd <- remDr$findElement("xpath", '//*[(@id = "password")]')
# pswd$clearElement()
# pswd$sendKeysToElement(list(keyring::key_get("sub", "juro7132"), key = "enter"))

#### for loop should start here
## create list to catch results
revenues <- list()
guo_list <- list()
shr_list <- list()

## re-start the search: here just for quick restart
remDr$goBack()
# You need to do the search manually (2-3 loops) until it gives you the option
# of restarting search again 
tic()
for (i in 24:length(dat$company)){ #seq_along(dat$company)
    ## Search one company:
    srch <- remDr$findElement("id", "search")
    
    ## repeating the search allows me to clean up the previous search, that is why this
    ## line is repeated, but second time with "enter"
    srch$sendKeysToElement(list(dat$company[i]))
    ## clear search: J221217: I may need to do this twice, it does not clear on the first round
    clr <- remDr$findElement("xpath", '//*[contains(concat( " ", @class, " " ), concat( " ", "clear", " " ))]')
    clr$clickElement()
    
    srch$sendKeysToElement(list(dat$company[i], key = "enter"))
    
    ## wait for a bit
    Sys.sleep(10)
    resp <- remDr$findElements("id", "quicksearch-results")
    opts <- remDr$findElements(
        "xpath",
        '//*[contains(concat( " ", @class, " " ), concat( " ", "name", " " ))]')
    
    if(length(opts) == 0) next ## If the company is not found, go to next iteration
    
    opts[[1]]$getElementText()
    opts[[1]]$clickElement() ## need to interveene here manually for first time
    #remDr$acceptAlert() #accepting alert does not work
    
    Sys.sleep(20)
    
    
    html <- remDr$getPageSource() |> 
        unlist() |> 
        read_html()
    
    tbls <- html |> html_elements("table") 
    idx <- which(tbls |> html_attr("class") == "ETBL contactInformation" )
    info <- tbls[idx] |> html_table()
    Sys.sleep(3)
    ## Extract revenues:
    drop_menus <- remDr$findElements(
        "xpath",
        '//*[contains(concat( " ", @class, " " ), concat( " ", "menu__view-selection-item-icon", " " ))]')
    drop_menus[[2]]$clickElement()
    idx <- html |> html_elements("a") |> 
        html_attr("href") == "#KEYFINANCIALS"
    opts <-  remDr$findElements("css",'a')
    opts[[which(idx)[1]]]$clickElement()
    Sys.sleep(10)
    
    html <- remDr$getPageSource() |> 
        unlist() |> 
        read_html()
    
    tbls <- html |> html_elements("table") 
    idx <- which(tbls |> html_attr("data-table-name") == "financial")[1]
    revenue <- ifelse(is.na(idx), "No data available for this company", tbls[idx] |> html_table())
    
    
    ## Go to shareholders: this method is unreliable because sometimes if there is missing info in one of the preceding boxes, the more button does not appear, so it is not the second but the first one. 
    # more <- remDr$findElements("xpath", '//*[contains(concat( " ", @class, " " ), concat( " ", "showMore", " " ))]')
    # more[[1]]$clickElement() # It's the second element on the list
    # Sys.sleep(10)
    
    ## this makes the menues visible (not sure if necessary)
    drop_menus <- remDr$findElements(
        "xpath",
        '//*[contains(concat( " ", @class, " " ), concat( " ", "menu__view-selection-item-icon", " " ))]')
    drop_menus[[9]]$clickElement()
    
    ## read the raw html
    html <- remDr$getPageSource() |> 
        unlist() |> 
        read_html()
    ## extract the a elements with href "#CONTROLLINGSHAREHOLDERS". I found the tag
    ## by studying the html on web inspector (Safari)
    idx <- html |> html_elements("a") |> 
        html_attr("href") == "#CONTROLLINGSHAREHOLDERS"
    shr_opt <- remDr$findElements("css",'a')
    shr_opt[[which(idx)[1]]]$clickElement()
    Sys.sleep(5)
    
    ## Extract ownership
    html <- remDr$getPageSource() |> 
        unlist() |> 
        read_html()
    
    # extract table
    tbls <- html |> html_elements("table") # it is table 5 in this case
    wt <- tbls |> html_attr("id")  # tells me which one programatically 
    wt <- which(wt == "Section_CONTROLLINGSHAREHOLDERS_InLines_OwnershipTable")[1]
    
    shr <- ifelse(is.na(wt), "There is no shareholder information", tbls[wt] |>
        html_table()) # shareholders
    
    ## Get the global ultimate owner. The table also contain IDs for orbis and BvD
    guo_ids <- tbls[1] |> html_table()
    
    ## save results
    revenues[[i]] <- revenue
    shr_list[[i]] <- shr
    guo_list[[i]] <- guo_ids
    
    ## re-start the search
    remDr$goBack()
}
toc()

remDr$refresh()

## Don't forget to close:
remDr$closeall()

## save the list objects for later cleaning
save(revenues, shr_list, guo_list, file = "data/bolivia_orbis.RData")

### Do this after all tables are downloaded
# shr <- shr[[1]] |> 
#     rename(name = X1, country = X3, type = X4, ownership_direct = X6, ownership_total = X7,
#            info_source = X8, info_date = X9, op_revenue_MUSD = X10, num_employees = X11) |> 
#     slice(-c(1,2), -n()) |> 
#     select(-starts_with("X"))

## To do:
## manage keys to avoid putting your user name.
## 
## 
## 
## 
#### Left overs ####
## ## Need to set up a profile in order to download files automatically
## Tip learned from: https://stackoverflow.com/questions/29759438/rselenium-popup
# prfl <- makeFirefoxProfile(list(
#     browser.download.dir = "/Users/juanrocha/Downloads/temp",
#     browser.download.folderList = 2L,
#     browser.download.manager.showWhenStarting = FALSE,
#     browser.helperApps.neverAsk.saveToDisk = "application/zip"))


## Download excel file with shareholders
# btn <- remDr$findElements("xpath", '//*[contains(concat( " ", @class, " " ), concat( " ", "button--icon-text", " " ))]')
# btn[[3]]$getElementText() # This is the excel button
# btn[[3]]$clickElement()

## Extract the ID: not necessary anymore, can get from tables step (same page as controlling shareholders)
# ids <- remDr$findElement("xpath", '//*[contains(concat( " ", @class, " " ), concat( " ", "state", " " ))] | //*[contains(concat( " ", @class, " " ), concat( " ", "variationValuePrim", " " ))]')
# ids <- ids$getElementText()

## Extract revenues: this is wrong because depending on the company, it comes in thousands or millions...which is not consistent
# rev <- remDr$findElement("xpath", '//*[contains(concat( " ", @class, " " ), concat( " ", "variationValuePrim", " " ))]')
# rev <- rev$getElementText()

## solving problems with RSelenium
## using code from: https://github.com/ropensci/RSelenium/issues/263

# library(wdman)
# selenium(retcommand=T)
