#' Get Census Weights for the 14 cities
#'
#' IBGE changed the weighting areas for 14 cities. This function specifically
#' deals with this nuisance.
#'
#' @param geo Logical indicating if Census weights should include a geometry column.
#' Defaults to `FALSE`
#' @param dir Directory path where the "14_municipios" data is stored
#'
#' @return Either a `tibble` (if `geo = FALSE`) or a spatial `sf` object
#' (if `geo = TRUE`).
get_census_14_muni <- function(geo = FALSE, dir = NULL) {

  if (is.null(dir)) {
    dir <- here::here("data-raw/microdata_14muni/data/")
  }

  # Get import dictionary
  dict <- microdadosBrasil::get_import_dictionary("CENSO", 2010, ft = "domicilios")
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
  #
  census_wgt <- census_estimate_wgt(domi)
  census_wgt = get_census_weights(census_wgt)

  if (geo) {

    wgt <- sf::st_make_valid(wgt10)

    wgt <- dplyr::mutate(
      wgt,
      code_weighting = as.character(code_weighting),
      code_muni = as.character(code_muni)
    )

    census_wgt <- dplyr::rename(census_wgt, code_weighting = code_weighting_area)
    census_wgt <- dplyr::mutate(
      census_wgt,
      code_weighting = as.character(code_weighting),
      code_muni = as.character(code_muni)
    )

    out <- dplyr::left_join(
      wgt10,
      census_wgt,
      by = c("code_weighting", "code_muni")
      )

    return(out)
  }

  return(census_wgt)

}
