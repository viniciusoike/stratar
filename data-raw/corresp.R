
library(sf)
library(dplyr)

dict <- readxl::read_excel(
  here::here("data-raw/composition_sectors.xls"),
  skip = 1,
  col_names = c("code_tract", "code_weighting")
  )

cities <- dict |>
  dplyr::mutate(code_muni = substr(code_tract, 1, 7)) |>
  dplyr::pull(code_muni) |>
  unique() |>
  as.numeric()

tracts <- lapply(
  cities,
  geobr::read_census_tract,
  simplified = FALSE,
  showProgress = FALSE
  )

tracts <- dplyr::bind_rows(tracts)
tracts <- dplyr::mutate(tracts, code_tract = as.character(code_tract))
tracts <- dplyr::left_join(tracts, dict, by = "code_tract")
library(dplyr)

test = tracts %>%
  dplyr::group_by(code_weighting) %>%
  dplyr::summarise(geom = st_union(.))
