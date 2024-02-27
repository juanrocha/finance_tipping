library(tidyverse)
library(RSelenium)
library(keyring)
library(rvest)
library(tictoc)

load("data/boats_owners.Rda")
owners <- df_boats |>
    filter(!is.na(reg_owner)) |> 
    pull(reg_owner) |> unique()

df_missing <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1TWdrM4ybY_9xYRfWcVL2KqBNTfw-Tyat6mEnzSXKCGU/edit#gid=322700622", sheet = 2)

df_missing <- df_missing |> 
    filter(!is.na(OrbisID))


## Initialize remote driver
d <- rsDriver(
    port = 4444L, browser = "firefox", extraCapabilities = list(acceptInsecureCerts = TRUE)) # should open a chrome
remDr <- d[["client"]]
#remDr$open()
tic()
remDr$navigate(url = "https://ezp.sub.su.se/login?url=https://orbis4.bvdinfo.com/ip")
toc() # >6s
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
# revenues <- list()
# guo_list <- list()
# shr_list <- list()

## re-start the search: here just for quick restart
remDr$goBack()
# You need to do the search manually (2-3 loops) until it gives you the option
# of restarting search again 
# guos_missing comes from script 17
owners <- guos_missing # only run for second batch, trying to recover companies missed on the first round

# car_dat comes from script 17, additional companies from Carmine et al dataset
owners <- car_owners

# df_missing comes from google drive, they are orbis ids recovered manually by Bianca
owners <- df_missing$OrbisID

# df for mining: 29k
owners <- mines

remDr$refresh()

for (i in 411:length(owners)){ #seq_along(dat$company)
    tic()
    ## Search one company:
    srch <- remDr$findElement("id", "search")
    # delete any left over from last iteration
    # walk(1:nchar(owners[1]), ~srch$sendKeysToElement(list(key = "backspace")))
    
    ## repeating the search allows me to clean up the previous search, that is why this
    ## line is repeated, but second time with "enter"
    srch$sendKeysToElement(list(owners[i]))
    ## clear search: J221217: I may need to do this twice, it does not clear on the first round
    clr <- remDr$findElement("xpath", '//*[contains(concat( " ", @class, " " ), concat( " ", "clear", " " ))]')
    clr$clickElement()
    
    srch$sendKeysToElement(list(owners[i], key = "enter"))
    
    ## wait for a bit
    Sys.sleep(10)
    resp <- remDr$findElements("id", "quicksearch-results")
    opts <- remDr$findElements(
        "xpath",
        '//*[contains(concat( " ", @class, " " ), concat( " ", "name", " " ))]')
    
    ## If the company is not found, go to next iteration
    if(length(opts) == 0) next 
    
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
    Sys.sleep(10)
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
    
    print(glue::glue("Company ", i, " completed, it took:\n"))
    toc()
}


remDr$refresh()

## Don't forget to close:
remDr$closeall()

## save the list objects for later cleaning
save(revenues, shr_list, guo_list, owners, file = "data/carmine_shareholders.RData")

## J231214: clean RAs

prbls <- owners |> is.na() 
revenues <- revenues[!prbls] 
shr_list <- shr_list[!prbls]
guo_list <- guo_list[!prbls]
owners <- owners[!prbls]
