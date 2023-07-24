id_cities <- geobr::read_municipality(year = 2010)
id_cities <- sf::st_drop_geometry(id_cities)

pop <- sidrar::get_sidra(608, variable = 93, classific = c('c2'), geo = "City")

pop <- janitor::clean_names(pop)
pop <- tibble::as_tibble(pop)

main_cities <- pop |>
  dplyr::filter(sexo == "Total", valor > 2 * 1e5) |>
  dplyr::select(code_muni = municipio_codigo, population = valor) |>
  dplyr::mutate(code_muni = as.numeric(code_muni)) |>
  dplyr::left_join(id_cities, by = "code_muni")

readr::write_csv(main_cities, "data-raw/main_cities.csv")
