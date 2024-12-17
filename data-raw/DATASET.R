## code to prepare `DATASET` dataset goes here

load("./data-raw/Boots.sf.RData")
usethis::use_data(Boots.sf, overwrite = TRUE)

load("./data-raw/FastFood.sf.RData")
usethis::use_data(FastFood.sf, overwrite = TRUE)

load("./data-raw/provinces_spain.rda")
usethis::use_data(provinces_spain, overwrite = TRUE)
