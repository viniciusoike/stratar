#' Compute Census Weights
#'
#' Uses 2010 Census household microdata to estimate strata weights. Currently
#' supports income and count weighting by number of bedrooms.
#'
#' @param state Two letter abbreviation of the state. This option assumes the Census
#' microdata has been downloaded and unziped in the `dir` directory. Either a single
#' state or 'all'.
#' @param dir Path to the Census microdata directory. Defaults to `NULL` assuming
#' standard file names.
#' @param export Logical indicating if results should be exported locally. Defaults
#' to `FALSE`.
#' @param variable One of `count`, `income`, or `all` (default).
#' @param tbl A `tibble` created by `census_estimate_wgt`
#'
#' @return A `data.table` with Census totals on columns by weighting area.
#' @export
get_census_weights <- function(
    state,
    tbl,
    dir = NULL,
    export = FALSE,
    variable = "all") {

  # Check inputs

  # Variable must be one of count, income, or all
  stopifnot(any(variable %in% c("count", "income", "all")))

  # State must be a valid 2 letter state abbreviation
  all_states <- c(
    "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS",
    "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC",
    "SE", "SP", "TO", "all"
  )

  if (!missing(tbl)) {
    wgt <- tbl
  } else {

  }


  if (!any(state %in% all_states)) {
    stop(paste("State must be one of: ", paste(all_states, collapse = ", ")))
  }

  if (any(state == "all")) {
    message("Reading Census microdata data for all states. This might take a while.")

    wgt <- parallel::mclapply(
      all_states,
      get_census_weights_single,
      dir = dir,
      variable = variable
      )

    # Export locally if export = TRUE
    if (isTRUE(export)) {
      path_out <- here::here(dir, glue::glue("wgt_dom.csv"))
      readr::write_csv(wgt, path_out)
      message("File exported to ", path_out)
    }

  } else {

    wgt <- get_census_weights_single(state, dir, variable)

    # Export locally if export = TRUE
    if (isTRUE(export)) {
      path_out <- here::here(dir, glue::glue("wgt_{state}_dom.csv"))
      readr::write_csv(wgt, path_out)
      message("File exported to ", path_out)
    }

  }

  return(wgt)

}

get_census_weights_single <- function(state, dir, variable) {

  # Import and process weights
  message(glue::glue("Importing Census microdata for: {state}."))
  dat <- census_import_state(state, dir)
  message("Computing weights...")
  wgt <- census_estimate_wgt(dat)

  if (variable != "all") {
    # Get the table
    out <- wgt[[variable]]

    # Create the code_muni column and pivot
    out <- out |>
      dplyr::mutate(code_muni = substr(code_weighting_area, 1, 7)) |>
      tidyr::pivot_wider(
        id_cols = c("code_weighting_area", "code_muni"),
        names_from = "hh_rooms",
        names_sort = TRUE,
        values_from = "n"
      )

  } else {

    # Stack tables together
    out <- dplyr::bind_rows(wgt[c("count", "income")], .id = "variable")
    # Create the code_muni column and pivot
    out <- out |>
      dplyr::mutate(code_muni = substr(code_weighting_area, 1, 7)) |>
      tidyr::pivot_wider(
        id_cols = c("code_weighting_area", "code_muni"),
        names_from = c("variable", "hh_rooms"),
        names_sort = TRUE,
        values_from = "n"
      )

    out <- dplyr::full_join(out, wgt[["agg"]], by = "code_weighting_area")

  }

}


#' Import household microdata from Census IBGE 2010
#'
#' @inheritParams get_census_weights
#' @param label Logical indicating if factor labels should be returned. Defaults
#' to `FALSE`.
#'
#' @return A `tibble`
#' @export
census_import_state <- function(state, dir = NULL, label = FALSE) {

  if (is.null(dir)) {
    dir <- here::here("data/br-ibge-censo/censo_2010_amostra")
  }
  # Get the path to the file based on the state abbreviation
  path <- list.files(dir, pattern = "dom\\.dta$", full.names = TRUE)
  path <- path[grep(paste0("_", state, "_"), path)]

  if (length(path == 1)) {
    # Import using haven
    census <- haven::read_dta(path)
  } else {
    # Import using haven
    census <- parallel::mclapply(path, haven::read_dta)
    census <- dplyr::bind_rows(census)
  }

  if (label) {
    # Return labelled tibble
    return(census)
  } else {
    # Return a non-labelled data.frame (faster data manipulation)
    census <- haven::zap_label(census)
    census <- haven::zap_labels(census)
    census <- haven::zap_formats(census)
    return(census)
  }

}

#' Estimate aggregate tables with Census Microdata
#'
#' Estimates totals and averages by number of bedrooms and weighting areas using
#' Census household microdata. By default, only permanent private households with
#' non-zero income are included.
#'
#' @details
#' The default option includes only apartments (`v4002 == 13`) and rented units
#' (`v0201 == 3`). As houses we include both single detached houses and houses in
#' condominiums (`v4002 %in% c(11, 12)`). For sales we include all privately owned
#' houses, whether they have a mortgage or note (`v0201 %in% c(1, 2)`).
#'
#' This function makes estimates faster by ignoring survey design and only providing
#' point estimates. That is, this function does not return estimates for standard
#' deviations. For standard deviations estimates consider using the `survey` package.
#'
#'
#' @param dat A `data.frame` or `data.table` imported with `census_import_census()`.
#' @param type One of `apartment` (default) or `house`.
#' @param operation One of `rent` (default) or `sale`.
#' @export
#' @return A named `list`
census_estimate_wgt <- function(dat, type = "apartment", operation = "rent") {

  stopifnot(is.data.frame(dat))
  stopifnot(any(type %in% c("apartment", "house")))
  stopifnot(any(operation %in% c("rent", "sale")))

  # Named vector with census variable dictionary
  census_variables <- c(
    "code_weighting_area" = "v0011",
    "weight" = "v0010",
    # Especie de unidade
    "hh_private" = "v4001",
    # Tipo de especie
    "hh_type" = "v4002",
    # Domicilio condicao de ocupacao
    "hh_ownership" = "v0201",
    # Rendimento mensal domiciliar em jul-2010
    "hh_income" = "v6529",
    # Numero, comodos como dormitorios
    "hh_rooms" = "v0204",
    # Aluguel (reais)
    "rent" = "v2011",
    # Aluguel (salarios minimos)
    "rent_mw" = "v2012"
  )

  # Rename and select columns
  dat <- dplyr::select(dat, dplyr::all_of(census_variables))

  # Convert columns to appropriate types
  fct_cols <- c("hh_type", "hh_ownership", "hh_private", "code_weighting_area")
  num_cols <- c("hh_rooms", "hh_income", "rent", "rent_mw", "weight")

  dat <- dat |>
    dplyr::mutate(dplyr::across(dplyr::all_of(fct_cols), as.factor)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), as.numeric))

  # Filter only valid households "private permanent households". This excludes
  # non-private households (such as prisons, or retirement homes) and inappropriate
  # housing
  dat <- dplyr::filter(dat, (hh_private == "1") | (hh_private == "01"))
  dat <- dplyr::filter(dat, hh_income > 0)

  tbl_household <- dat |>
    dplyr::summarise(
      n_household = sum(weight, na.rm = TRUE), .by = "code_weighting_area"
    )

  dat <- dplyr::left_join(dat, tbl_household, by = "code_weighting_area")
  # Truncate house rooms and then convert to factor
  dat <- dplyr::mutate(dat, hh_rooms = ifelse(hh_rooms >= 4, 4, hh_rooms))
  dat <- dplyr::mutate(dat, hh_rooms = factor(hh_rooms))

  # Filter rows
  if (type == "apartment") {
    sub <- dplyr::filter(dat, hh_type == "13")
  } else if (type == "house") {
    sub <- dplyr::filter(dat, hh_type %in% c("11", "12"))
  }

  if (operation == "rent") {
    sub <- dplyr::filter(sub, hh_ownership == "3")
  } else if (operation == "sale") {
    sub <- dplyr::filter(sub, hh_ownership %in% c("1", "2"))
  }

  # Compute contingency-tables using weights (much faster than survey::svytotal)
  # If confidence-interval is needed use survey
  count  <- stats::xtabs(weight ~ code_weighting_area + hh_rooms, data = sub)
  income <- stats::xtabs(hh_income * weight ~ code_weighting_area + hh_rooms, data = sub)

  # Estimate total number of households + avg. income
  tbl_summary <- sub |>
    dplyr::filter(hh_income > 0) |>
    dplyr::summarise(
      avg_income = stats::weighted.mean(hh_income, weight),
      avg_rent = stats::weighted.mean(rent, weight),
      avg_rent_mw = stats::weighted.mean(rent_mw, weight),
      total_household = mean(n_household),
      .by = "code_weighting_area"
    )
  # Estimate shares
  tbl_shares <- dat |>
    dplyr::mutate(
      is_rental = ifelse(hh_ownership == "3", 1L, 0L),
      is_apartment = ifelse(hh_type == "13", 1L, 0L),
      is_owned = ifelse(hh_ownership %in% c("1", "2"), 1L, 0L),
      is_house = ifelse(hh_type %in% c("11", "12"), 1L, 0L)
    ) |>
    dplyr::filter(hh_income > 0) |>
    dplyr::summarise(
      share_rental = sum(is_rental) / dplyr::n(),
      share_apartment = sum(is_apartment) / dplyr::n(),
      share_owned = sum(is_owned) / dplyr::n(),
      share_house = sum(is_house) / dplyr::n(),
      .by = "code_weighting_area"
    )
  # Full join both the summary and the shares table
  agg <- dplyr::full_join(tbl_summary, tbl_shares, by = "code_weighting_area")
  # Return output as a list of tibbles
  out <- list(agg = agg, count = count, income = income)
  out <- lapply(out, tibble::as_tibble)
  return(out)

}
