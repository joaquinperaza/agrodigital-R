# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



set_token <- function(token) {
  if (is.null(token) || !nzchar(token)) {
    stop("API token is missing. Please enter a valid token.")
  } else {
    options("api_token" = token)
  }
}

get_api_token <- function() {
  token <- getOption("api_token")
  if (is.null(token)) {
    stop("API token is not set. Please use `set_api_token()` to set it.")
  }
  return(token)
}

check_api_token <- function() {
  token <- getOption("api_token")
  if (is.null(token)) {
    stop("API token is not set. Please use `set_api_token()` to set it.")
  }
}



#' Download data from the Agrodigital.io API
#'
#' @param station_id The ID of the station to download data for.
#' @return A data frame containing the downloaded data.
#' @export
download_data <- function(station_id) {
  # Check if API token is set
  check_api_token()

  # Base URL for the API
  base_url <- "https://agrodigital.io/api/iot/data"

  # Prepare the URL
  url <- paste0(base_url, "?station=", station_id)

  # Get the API token
  token <- get_api_token()

  # Send a GET request to the API
  response <- httr::GET(url, add_headers(Authorization = paste0("Token ", token)))

  # Check the status of the response
  if (httr::http_error(response)) {
    print(response)
    stop("An error occurred while downloading the data.")
  }

  # Parse the response as JSON
  data <- httr::content(response, as = "parsed")

  # Convert the data to a data frame
  df <- as.data.frame(data)

  return(df)
}

#' Query rasters from the AgroDigital API
#'
#' @param field_id The ID of the field to download data for.
#' @param sources Vector of source types to filter by.
#' @param date_start Optional start date to filter by.
#' @param date_end Optional end date to filter by.
#' @return A data frame containing the downloaded data.
#' @export
query_rasters <- function(field_id, sources, date_start = NULL, date_end = NULL) {
  # Check if API token is set
  check_api_token()

  # Base URL for the API
  base_url <- "https://agrodigital.io/api/rasters"

  # Prepare the URL
  url <- paste0(base_url, "?field=", field_id)

  # Append sources to the URL
  for (source in sources) {
    url <- paste0(url, "&sources[]=", source)
  }

  # Get the API token
  token <- get_api_token()

  # Send a GET request to the API
  response <- httr::GET(url, add_headers(Authorization = paste0("Token ", token)))

  # Check the status of the response
  if (httr::http_error(response)) {
    print(response)
    stop("An error occurred while downloading the data.")
  }

  # Parse the response as JSON
  data <- httr::content(response, as = "parsed")

  # Convert the data to a data frame
  df <- as.data.frame(data)

  # If date filtering parameters are provided, filter the data frame
  if (!is.null(date_start) || !is.null(date_end)) {
    # Convert the 'date' column to Date format
    df$date <- lubridate::ymd(df$date)

    # Apply date filters
    if (!is.null(date_start)) {
      df <- df[df$date >= lubridate::ymd(date_start),]
    }
    if (!is.null(date_end)) {
      df <- df[df$date <= lubridate::ymd(date_end),]
    }
  }

  return(df)
}

#' Load a GeoTIFF file and convert it to a matrix
#'
#' @param url The URL of the GeoTIFF file.
#' @return A matrix representation of the GeoTIFF data.
#' @export
load_geotiff <- function(url) {
  # Download the file to a temporary location
  temp_file <- tempfile(fileext = ".tif")
  download.file(url, temp_file, mode = "wb")

  # Load the GeoTIFF file as a raster
  r <- raster::raster(temp_file)

  # Convert the raster to a matrix
  m <- raster::as.matrix(r)

  return(m)
}


#' Process a list of rasters and convert them to matrices
#'
#' @param rasters A data frame of rasters, such as the output from query_rasters().
#' @return A list of matrices, one for each raster in the input.
#' @export
download_rasters <- function(rasters) {
  # Initialize an empty list to store the matrices
  matrices <- list()

  # Loop over the rows of the raster data frame
  for (i in 1:nrow(rasters)) {
    # Get the URL of the current raster
    url <- rasters$url[i]

    # Load the GeoTIFF file and convert it to a matrix
    m <- load_geotiff(url)

    # Add the matrix to the list
    matrices[[i]] <- m
  }

  return(matrices)
}
