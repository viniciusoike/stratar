
get_ftp_url <- function() {

  # Define the URL of the FTP server
  ftp_url <- "ftp://geoftp.ibge.gov.br/recortes_para_fins_estatisticos/malha_de_areas_de_ponderacao/censo_demografico_2010/municipios_areas_redefinidas/"

  # Fetch the file listing from the FTP server
  file_listing <- RCurl::getURL(ftp_url, dirlistonly = TRUE)
  file_listing <- strsplit(file_listing, "\n")[[1]]

  out <- paste0(ftp_url, file_listing)

  return(out)

}

read_shapefile <- function(url, outdir = NULL) {

  if (is.null(outdir)) {
    outdir <- tempdir()
  }

  for (i in seq_along(url)) {

    zip_file <- basename(url)[[i]]
    # Try to download the file using download.file() function
    file <- try(
      download.file(url[[i]], destfile = file.path(outdir, zip_file), mode = "wb")
    )

    if (inherits(file, "try-error")) {
      message("Download failed for: ", zip_file)
      next
    }

  }

  files <- list.files(outdir, pattern = "\\.zip$", full.names = TRUE)

  for (i in seq_along(files)) {

    try(zip::unzip(files[[i]], exdir = outdir))

  }

  path_shape <- list.files(outdir, pattern = "\\.shp$", full.names = TRUE)
  shape <- lapply(path_shape, sf::st_read, quiet = TRUE, crs = 4674)

  return(shape)

}

clean_geometries <- function(shp) {

  # Select and rename columns
  new_names <- c("code_weighting" = "CD_APONDE")
  shp <- dplyr::select(shp, dplyr::all_of(new_names))
  # Convert code_weighting to character
  shp <- dplyr::mutate(shp, code_weighting = as.character(code_weighting))

  shp <- sf::st_make_valid(shp)

  # Check if geometry is POLYGON and convert to MULTIPOLYGON
  if (any(sf::st_geometry_type(shp) == "POLYGON")) {
    shp <- sf::st_cast(shp, to = "MULTIPOLYGON")

  }

  shp <- sf::st_transform(shp, crs = 4674)

}

urls <- get_ftp_url()
test = read_shapefile(urls, outdir = here::here("data-raw", "ibge"))
clean = lapply(test, \(x) try(clean_geometries(x)))

area_ponderacao <- geobr::read_weighting_area(year = 2010, simplified = FALSE)

fixed <- dplyr::bind_rows(clean[-c(7, 8)])
fixed <- dplyr::rename(fixed, geom = geometry)
fixed <- dplyr::mutate(fixed, code_muni = substr(code_weighting, 1, 7))
fixed <- sf::st_transform(fixed, crs = 4674)

dim_city <- area_ponderacao |>
  sf::st_drop_geometry() |>
  dplyr::select(-"code_weighting") |>
  dplyr::distinct()

fixed <- dplyr::left_join(fixed, dim_city, by = "code_muni")

rmcity <- unique(fixed$code_muni)

sf::st_crs(area_ponderacao) == sf::st_crs(fixed)

wgt10 <- area_ponderacao |>
  dplyr::filter(!(code_muni %in% rmcity)) |>
  dplyr::bind_rows(fixed)

index_cities <- c(
  3106200, 5300108, 4106902, 4314902, 3304557, 3550308, 2611606, 2927408
  )

wgt10 <- dplyr::filter(wgt10, code_muni %in% index_cities)

usethis::use_data(wgt10, overwrite = TRUE)
