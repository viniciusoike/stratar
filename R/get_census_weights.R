#' Compute Census Weights
#'
#' Uses 2010 Census household microdata to estimate strata weights. Currently
#' supports income and count weighting by number of bedrooms.
#'
#' @param state Two letter abbreviation of the state
#' @param dir Path to the Census microdata directory. Defaults to `NULL` assuming
#' standard file names.
#' @param export Logical indicating if results should be exported locally. Defaults
#' to `FALSE`.
#' @param variable One of `count`, `income`, or `all` (default).
#'
#' @return A `data.table` with Census totals on columns by weighting area.
#' @export
get_census_weights <- function(
    state = "AC",
    dir = NULL,
    export = FALSE,
    variable = "all") {

  # Check inputs

  # State must be a valid 2 letter state abbreviation
  all_states <- c(
    "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS",
    "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC",
    "SE", "SP", "TO"
  )

  if (!any(state %in% all_states)) {
    stop(paste("State must be one of: ", paste(all_states, collapse = ", ")))
  }

  # Variable must be one of count, income, or all
  stopifnot(any(variable %in% c("count", "income", "all")))

  # Import and process weights

  message(glue::glue("Importing Census microdata for: {state}."))
  dat <- census_import_state(state, dir)
  message("Computing weights...")
  wgt <- census_estimate_wgt(dat)

  if (variable != "all") {
    # Get the table
    out <- wgt[[variable]]
    # Create the code_muni column
    out[, code_muni := substr(code_weighting_area, 1, 7)]
    # Pivot table
    out <- data.table::dcast(
      out,
      code_weighting_area + code_muni ~ hh_rooms,
      value.var = "Freq"
    )

  } else {

    # Stack tables together
    out <- data.table::rbindlist(wgt[c("count", "income")], idcol = "variable")
    # Create the code_muni column
    out[, code_muni := substr(code_weighting_area, 1, 7)]
    # Pivot table
    out <- data.table::dcast(
      out,
      code_weighting_area + code_muni ~ variable + hh_rooms,
      value.var = "Freq"
    )

  }

  if (isTRUE(export)) {
    path_out <- here::here(dir, glue::glue("wgt_{state}_dom.csv"))
    data.table::fwrite(out, path_out)

    message("File exported to ", path_out)

  }

  return(out)

}

census_import_state <- function(state, dir = NULL, label = FALSE) {

  if (is.null(dir)) {
    dir <- here::here("data/br-ibge-censo/censo_2010_amostra")
  }
  # Get the path to the file based on the state abbreviation
  path <- list.files(dir, pattern = "dom\\.dta$", full.names = TRUE)
  path <- path[grep(paste0("_", state, "_"), path)]

  # Check if only one file was found
  stopifnot(length(filenm) >= 1)
  # Check if file exists
  stopifnot(file.exists(path))

  # Import using haven
  census <- haven::read_dta(path)

  if (label) {
    # Return labelled tibble
    return(census)
  } else {
    # Return a non-labelled data.table (faster data manipulation)
    census <- data.table::setDT(census)
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
#'
#' @return A named list
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

  # Convert to data.table
  if (!inherits(dat, "data.table")) {
    data.table::setDT(dat)
  }

  # Rename and select columns
  dat <- dat[, ..census_variables]
  setnames(dat, names(dat), names(census_variables))

  # Filter only valid households "private permanent households". This excludes
  # non-private households (such as prisons, or retirement homes) and inappropriate
  # housing
  dat <- dat[hh_private == 1]
  # Remove zero income households. This creates an upwards bias in estimates
  # but is more useful for index analysis
  dat <- dat[hh_income > 0]

  # Find total number of households per strata
  dat[, n_household := sum(weight, na.rm = TRUE), by = "code_weighting_area"]

  # Truncate number of rooms
  dat[, hh_rooms := ifelse(hh_rooms >= 4, 4, hh_rooms)]

  # Convert columns to appropriate types
  fct_cols <- c("hh_rooms", "hh_type", "hh_ownership", "code_weighting_area")
  num_cols <- c("hh_income", "rent", "rent_mw")

  dat <- dat[, (fct_cols) := lapply(.SD, as.factor), .SDcols = fct_cols]
  dat <- dat[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

  # dat[, hh_rooms := factor(hh_rooms)]
  # dat[, hh_type := factor(hh_type)]
  # dat[, code_weighting_area := factor(code_weighting_area)]

  # Filter rows
  if (type == "apartment") {
    sub <- dat[hh_type == "13"]
  } else if (type == "house") {
    sub <- dat[hh_type %in% c("11", "12")]
  }

  if (operation == "rent") {
    sub <- sub[hh_ownership == "3"]
  } else if (operation == "sale") {
    sub <- sub[hh_ownership %in% c("1", "2")]
  }

  # Remove NAs
  # sub <- na.omit(sub)

  # Compute contingency-tables using weights (much faster than survey::svytotal)
  # If confidence-interval is needed use survey
  count  <- stats::xtabs(weight ~ code_weighting_area + hh_rooms, data = sub)
  income <- stats::xtabs(hh_income * weight ~ code_weighting_area + hh_rooms, data = sub)

  # Convert tables to data.table
  count <- data.table::setDT(as.data.frame(count))
  income <- data.table::setDT(as.data.frame(income))

  # Estimate total number of households + avg. income
  tbl_summary <- sub[
    hh_income > 0,
    .(avg_income = stats::weighted.mean(hh_income, weight),
      avg_rent = stats::weighted.mean(rent, weight),
      avg_rent_mw = stats::weighted.mean(rent_mw, weight),
      total_household = mean(n_household)),
    by = "code_weighting_area"
  ]

  # Estimate shares
  dat[, is_rental := 0][hh_ownership == "3", is_rental := 1]
  dat[, is_apartment := 0][hh_type == "13", is_apartment := 1]
  dat[, is_owned := 0][hh_ownership %in% c("1", "2"), is_rental := 1]
  dat[, is_house := 0][hh_type %in% c("11", "12"), is_apartment := 1]

  tbl_shares <- dat[
    hh_income > 0,
    .(
      share_rental = sum(is_rental) / .N,
      share_apartment = sum(is_apartment) / .N,
      share_owned = sum(is_owned) / .N,
      share_house = sum(is_house) / .N
    ),
    by = "code_weighting_area"
  ]

  # Full join both the summary and the shares table
  agg <- merge(tbl_summary, tbl_shares, all.x = TRUE, all.y = TRUE)
  out <- list(agg = agg, count = count, income = income)
  return(out)

}
