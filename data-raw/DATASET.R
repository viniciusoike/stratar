## code to prepare `DATASET` dataset goes here

bra_main_cities <- readr::read_csv("data-raw/main_cities.csv")

dict_census <- readr::read_csv("data-raw/dict_census.csv")


usethis::use_data(bra_main_cities, overwrite = TRUE)
usethis::use_data(dict_census, internal = TRUE, overwrite = TRUE)
