# _targets.R file
library(targets)

source("R/functions.R")
tar_option_set(packages = c("tidyverse", "rvest"))

list(tar_target(url, "https://oatcookies.neocities.org/kjb-scum"),
     tar_target(data, get_data(url)),
     tar_target(df, clean_data(data)),
     tar_target(save, save_data(df))
     )

