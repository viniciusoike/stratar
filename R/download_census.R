#' Download and optionally unzip Census data
#'
#' @param type One of `'universo'` or `'amostra'`
#' @param year Only accepts 2010
#' @param download Logical indicating if data should be downloaded locally. Defaults
#' to TRUE
#' @param dir Directory where the data should be downloaded. Defaults to `"data-raw"`
#' @param unzip Logical indicating if downloaded zip files should be unzipped. Defaults
#' to `TRUE`.
#' @export
download_census <- function(
    type = "universo",
    year = 2010,
    download = TRUE,
    dir = NULL,
    unzip = TRUE) {

  if (type == "universo") {
    ftpurl <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios/"
    fld <- here::here("data-raw", "universo")
  } else if (type == "amostra") {
    ftpurl <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/"
    fld <- here::here("data-raw", "microdata")
  }

  if (!dir.exists(fld)) { dir.create(fld) }

  # Get the name of all files to download
  files <- RCurl::getURL(ftpurl, dirlistonly = TRUE)

  # Extract file names using regular expressions
  files <- stringr::str_remove(files, "(\\r|\\n)")
  files <- stringr::str_split(files, " +")[[1]]
  files <- files[stringr::str_detect(files, "\\.zip")]
  file_name <- stringr::str_extract(files, "(?<=href=\")[^\\\"]+")

  # Function to download a single file
  download_file <- function(file_name) {
    url <- paste0(ftpurl, file_name)
    dest_file <- here::here(fld, file_name)

    if (file.exists(dest_file)) {
      message("File ", file_name, " already exists at: ", dest_file)
    } else {
      bin <- RCurl::getBinaryURL(url)
      writeBin(bin, dest_file)
    }
  }

  # Download all files using map()
  if (download) {
    out <- purrr::map(file_name, download_file)
    message("Files download to ", fld)

    if (unzip) {
      # Get all zipped file names and paths
      zip_names <- list.files(fld, pattern = "[A-Z]{2}\\.zip$")
      path_zip <- here::here(fld, zip_names)
      # Unzip all txt/csv files into respective folders
      purrr::map(path_zip, unzip_files, force = TRUE)
    }

  }

}

# Unzip files
unzip_files <- function(path, output_path = NULL, force = FALSE) {

  # List all files inside the zip
  list_files <- utils::unzip(path, list = TRUE)
  files <- list_files[["Name"]]
  # Get only the csv/txt files
  files <- files[stringr::str_detect(files, "(txt$)|(csv$)")]

  # Unzip these files

  if (is.null(output_path)) {
    # Get the name of the zipfile
    state_abbrev <- stringr::str_remove(basename(path), "\\..+")
    output_path <- here::here(dirname(path), state_abbrev)
  }

  if (!dir.exists(output_path)) {
    warning("The directory: ", output_path, " does not exist.")
    if (force) {
      warning("Selected `force = TRUE`. Creating ", output_path)
      dir.create(output_path)
    } else {
      stop("Invalid output path.")
    }
  }

  utils::unzip(
    path,
    files = files,
    exdir = output_path,
    junkpaths = TRUE,
    overwrite = TRUE
    )

  message("Files unzipped to ", output_path)

}

# library(stringr)
# library(purrr)
#
# # Define the url of IBGE's FTP server
# ftpurl <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios/"
#
# # Get the name of all files to download
# files <- RCurl::getURL(ftpurl, dirlistonly = TRUE)
#
# # Extract file names using regular expressions
# files <- str_remove(files, "(\\r|\\n)")
# files <- str_split(files, " +")[[1]]
# files <- files[str_detect(files, "\\.zip")]
# file_name <- str_extract(files, "(?<=href=\")[^\\\"]+")
#
# # Function to download a single file
# download_file <- function(file_name) {
#   url <- paste0(ftpurl, file_name)
#   dest_file <- here::here("data-raw", file_name)
#   bin <- RCurl::getBinaryURL(url)
#   writeBin(bin, dest_file)
# }
#
# # Create directory
# dir.create(here::here("data-raw"))
# # Download all files using map()
# map(file_name, download_file)
#
# # Download Census IDs
