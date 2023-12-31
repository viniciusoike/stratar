#' Get Census Weights for the 14 cities
#'
#' IBGE changed the weighting areas for 14 cities. This function specifically
#' deals with this nuisance.
#'
#' @param geo Logical indicating if Census weights should include a geometry column.
#' Defaults to `FALSE`
#' @param dir Directory path where the "14_municipios" data is stored
#' @export
#' @return Either a `tibble` (if `geo = FALSE`) or a spatial `sf` object
#' (if `geo = TRUE`).
get_census_14_muni <- function(geo = FALSE, dir = NULL) {

  if (is.null(dir)) {
    dir <- here::here("data-raw/microdata_14muni/data/")
  }

  # Get import dictionary
  dict <- dict_census
  # Import data
  domi <- readr::read_fwf(
    file = here::here(dir, "Amostra_Domicilios_14munic.txt"),
    col_positions = readr::fwf_positions(
      start = dict$int_pos,
      end = dict$fin_pos,
      col_names = dict$var_name
      ),
    col_types = paste(dict$col_type, collapse = ""),
    n_max = -1L,
    na = c("", "NA")
  )

  # Convert column names to lowercase
  names(domi) <- tolower(names(domi))
  census_wgt <- census_estimate_wgt(domi)

  # Stack tables together
  out <- dplyr::bind_rows(census_wgt[c("count", "income")], .id = "variable")

  out <- out |>
    dplyr::mutate(code_muni = substr(code_weighting_area, 1, 7)) |>
    tidyr::pivot_wider(
      id_cols = c("code_weighting_area", "code_muni"),
      names_from = c("variable", "hh_rooms"),
      names_sort = TRUE,
      values_from = "n"
    )

  tbl_census <- dplyr::full_join(out, census_wgt[["agg"]], by = "code_weighting_area")


  if (geo) {

    wgt_area <- get_wgt_area("all")
    wgt <- dplyr::filter(wgt_area, code_muni %in% unique(tbl_census$code_muni))
    wgt <- sf::st_make_valid(wgt)

    wgt <- wgt |>
      dplyr::mutate(
        code_weighting = as.character(.data[["code_weighting"]]),
        code_muni = as.character(.data[["code_muni"]])
      )

    tbl_census <- tbl_census |>
      dplyr::rename(c("code_weighting" = "code_weighting_area")) |>
      dplyr::mutate(
        code_weighting = as.character(.data[["code_weighting"]]),
        code_muni = as.character(.data[["code_muni"]])
      )

    out <- dplyr::left_join(wgt, tbl_census, by = c("code_weighting", "code_muni"))
    return(out)
  }

  return(tbl_census)

}
