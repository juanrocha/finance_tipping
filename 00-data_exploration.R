library(tidyverse)

# comtrade data HS rev 1992-2005, depth 0
path <- "~/Documents/Projects/DATA/ComTrade/Exports_0"
files <- list.files(path)

out <- map(paste(path, files, sep = "/"), read_csv)
