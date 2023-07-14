library(tidyverse)
library(RSelenium)
library(keyring)
library(rvest)
library(tictoc)


load("data/boats.Rda")
#load("data/vessels/vessels-6001-7000.RData")
df_boats


## Initialize remote driver
d <- rsDriver(
    port = 4445L, browser = "firefox") # should open a chrome
remDr <- d[["client"]]
#remDr$open()
tic()
remDr$navigate(url = "https://www.seasearcher.com")
toc() #

btn <- remDr$findElement("xpath",'//*[contains(concat( " ", @class, " " ), concat( " ", "lli-flex-row--justify-centered", " " ))]')
btn$clickElement()

usr <- remDr$findElement("id", "thePage:siteTemplate:j_id27:username")
usr$sendKeysToElement(list("frida.bengtsson@su.se"))


pwd <- remDr$findElement("id", 'thePage:siteTemplate:j_id27:password')
pwd$sendKeysToElement(list(keyring::key_get("maritime", "frida.bengtsson@su.se"), key = "enter"))
Sys.sleep(6)

btn <- remDr$findElement("css", '.lli-nav__link')
btn$clickElement()

# owners <- list()
# features <- list()

remDr$refresh()
Sys.sleep(5)

j = 16000# last iteration in the last saved file.

tic()
for (i in 761:875){
    
    srch <- remDr$findElement("class", "lli-searchform__input")
    srch$clickElement()
    # delete any left over from last iteration
    walk(1:9, ~srch$sendKeysToElement(list(key = "backspace")))
    
    srch$sendKeysToElement(list(df_boats$boats[[i + j]] |> as.character()))
    # gets the options from the list that appears on the search pane
    Sys.sleep(18)
    
    opts <- remDr$findElement("class","lli-typeahead-options__option")
    
    ## Test if the vessel is found, if not go to next
    if("No Results Found" == opts$getElementText() |> unlist()) {
        walk(1:9, ~srch$sendKeysToElement(list(key = "backspace")))
        next
        }
           
    opts$clickElement() # click the first one)

    Sys.sleep(15)
    
    ## Get html of all info on vessel:
    html <- remDr$getPageSource() |> 
        unlist() |> 
        read_html()
    # this captures the table with num of beneficial owners
    tbls <- html |> html_elements("table") 
    owners[[i]] <-  tbls[1] |> html_table() 
    
    f <- html |> html_elements(".mb1") |> 
        html_elements("dl") |> 
        html_elements("dt") |> 
        html_text2()
    v <- html |> html_elements(".mb1") |> 
        html_elements("dl") |> 
        html_elements("dd") |> 
        html_text2()

    features[[i]] <- tibble(
        feature = f,
        values = v[1:length(f)])
    
    rm(f,v,tbls) # clean up
    print(glue::glue("Vessel", {i}, "done, next...", .sep = " "))
    
    remDr$goBack()
    Sys.sleep(10)

}
toc() #30min for 40+ records.

save(owners, features, file = "data/vessels/vessels-16001-end.RData")
## actually 999

remDr$close()
