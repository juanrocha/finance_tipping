library(tidyverse)
library(RSelenium)
library(keyring)
library(rvest)
library(tictoc)


load("data/boats.Rda")
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
Sys.sleep(30)

btn <- remDr$findElement("css", '.lli-nav__link')
btn$clickElement()

srch <- remDr$findElement("class", "lli-searchform__input")
srch$clickElement()
srch$sendKeysToElement(list(df_boats$boats[[1]] |> as.character()))
#srch$sendKeysToElement(list(key = "down",key = "enter"))
# gets the options from the list that appears on the search pane
opts <- remDr$findElement("class","lli-typeahead-options__option")
opts$clickElement() # click the first one

## Get html of all info on vessel:
html <- remDr$getPageSource() |> 
    unlist() |> 
    read_html()

tbls <- html |> html_elements("table") 
info <- tbls[1] |> html_table()



remDr$goBack()
