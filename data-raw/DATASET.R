## code to prepare `DATASET` dataset goes here

bra_main_cities <- readr::read_csv("data-raw/main_cities.csv")

usethis::use_data(bra_main_cities, overwrite = TRUE)
