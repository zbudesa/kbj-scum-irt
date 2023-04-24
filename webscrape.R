library(tidyverse)

url <- "https://oatcookies.neocities.org/kjb-scum"

tables <- url %>% 
  read_html() %>% 
  html_table()

tables[1]

df <- rbind(tables[[1]] %>% transmute(`Episode` = `Epi­sode`,
                                      across(2:5, as.double)),
            tables[[4]] %>% transmute(`Episode` = `Season 2`,
                                      across(2:5, as.double))
            )


