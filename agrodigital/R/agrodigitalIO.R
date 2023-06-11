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
#' @param type Type string to filter eg. 'NDVI'.
#' @param date_start Optional start date to filter by.
#' @param date_end Optional end date to filter by.
#' @return A data frame containing the downloaded data.
#' @export
query_rasters <- function(field_id, sources, type = NULL, date_start = NULL, date_end = NULL) {
  # Check if API token is set
  check_api_token()
  cat("Downloading .")

  # Base URL for the API
  base_url <- "https://agrodigital.io/api/rasters"
  base_url_index <- "https://agrodigital.io/api/indexes"

  # Prepare the URL
  url <- paste0(base_url, "?field=", field_id)
  url_index <- paste0(base_url_index, "?field=", field_id)

  # Append sources to the URL
  for (source in sources) {
    url <- paste0(url, "&sources[]=", source)
    url_index <- paste0(url_index, "&sources[]=", source)
  }
  cat(".")

  # Get the API token
  token <- get_api_token()
  cat(".")

  # Send a GET request to the API
  response <- httr::GET(url, add_headers(Authorization = paste0("Token ", token)))
  cat(".")

  # Check the status of the response
  if (httr::http_error(response)) {
    print(response)
    stop("An error occurred while downloading the data.")
  }
  cat(".")

  # Parse the response as JSON
  data <- httr::content(response, as = "parsed")
  cat(".")

  # Convert the data to a data frame
  df <- as.data.frame(matrix(unlist(data), nrow=length(data), byrow=T))
  cat(".")

  # Rename the columns
  colnames(df) <- names(data[[1]])
  cat(".")

  # Get the index data
  response_index <- httr::GET(url_index, add_headers(Authorization = paste0("Token ", token)))
  cat(".")

  # Check the status of the response
  if (httr::http_error(response_index)) {
    print(response_index)
    stop("An error occurred while downloading the index data.")
  }
  cat(".")

  # Parse the response as JSON
  data_index <- httr::content(response_index, as = "parsed")
  cat(".")

  # Convert the data to a data frame
  df_index <- as.data.frame(matrix(unlist(data_index), nrow=length(data_index), byrow=T))
  cat(".")

  # Rename the columns
  colnames(df_index) <- names(data_index[[1]])
  cat(".")

  # Convert the 'date' columns to Date format
  df$date <- lubridate::ymd(df$date)
  df_index$date <- lubridate::ymd(df_index$date)
  cat(".")
  # Keep only the rows in df where the date is also present in df_index
  df <- df[df$date %in% df_index$date, ]


  if (!is.null(type)) {
    df <- df[df$type==type,]
  }
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
    cat(".")
  }
  cat(".OK!\n")
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
  # Get the number of cores
  num_cores <- parallel::detectCores()

  # Create a cluster of workers
  cl <- parallel::makeCluster(num_cores)

  # Export the load_geotiff function and the rasters$url variable to the workers
  parallel::clusterExport(cl, list("load_geotiff", "rasters"))

  # Download the rasters in parallel
  matrices <- parallel::parLapply(cl, rasters$url, load_geotiff)

  # Stop the cluster of workers
  parallel::stopCluster(cl)

  # Add the matrices as a new column in the rasters dataframe
  rasters$matrix <- matrices

  return(rasters)
}

query_iot_stations <- function() {
  # Check if API token is set
  check_api_token()

  # Base URL for the API
  base_url <- "https://agrodigital.io/api/iot/stations"

  # Get the API token
  token <- get_api_token()

  # Send a GET request to the API
  response <- httr::GET(base_url, add_headers(Authorization = paste0("Token ", token)))

  # Check the status of the response
  if (httr::http_error(response)) {
    print(response)
    stop("An error occurred while downloading the data.")
  }

  # Parse the response as JSON
  data <- httr::content(response, as = "parsed")

  # Create a data frame from the main level of the data
  df <- as.data.frame(do.call(rbind, data))

  # Flatten the 'location' column into separate columns for each of its fields
  df <- cbind(df, as.data.frame(do.call(rbind, df$location)))

  # Flatten the 'last_data' column into separate columns for each of its fields
  df <- cbind(df, as.data.frame(do.call(rbind, df$last_data)))

  # Remove the original 'location' and 'last_data' columns
  df$location <- NULL
  df$last_data <- NULL
  df$type <- NULL

  # Reset row names
  rownames(df) <- NULL

  return(df)
}

query_iot_data <- function(station_id) {
  # Get the API token
  token <- get_api_token()

  # Base URL for the API
  base_url <- "https://agrodigital.io/api/iot/data/"

  # Prepare the URL
  url <- paste0(base_url, "?station=", station_id)

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
  df <- do.call(rbind, lapply(data, function(x) {
    as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)
  }))

  # Reset row names
  rownames(df) <- NULL

  return(df)
}

