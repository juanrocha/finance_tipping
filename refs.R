library(tidyverse)

txt <- read_lines(file = "paper/manuscript.Rmd")

txt |> str_subset("\\[*\\]")

refs


system("curl -LH 'Accept: application/x-bibtex' https://www.doi.org/10.1126/science.1156401")
