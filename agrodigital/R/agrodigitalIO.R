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

  # Prepare the URL
  url <- paste0(base_url, "?field=", field_id)

  # Append sources to the URL
  for (source in sources) {
    url <- paste0(url, "&sources[]=", source)
  }

  url_d <- paste0(url, "&discard=true")

  cat(".")

  # Get the API token
  token <- get_api_token()
  cat(".")

  # Send a GET request to the API
  response <- httr::GET(url, add_headers(Authorization = paste0("Token ", token)))
  cat(".")

  # Send a GET request to the API
  response2 <- httr::GET(url_d, add_headers(Authorization = paste0("Token ", token)))
  cat(".")

  # Check the status of the response
  if (httr::http_error(response)) {
    print(response)
    stop("An error occurred while downloading the data.")
  }
  if (httr::http_error(response2)) {
    print(response2)
    stop("An error occurred while downloading the data.")
  }
  cat(".")

  # Parse the response as JSON
  data <- httr::content(response, as = "parsed")
  cat(".")
  data2 <- httr::content(response2, as = "parsed")
  cat(".")

  # Convert the data to a data frame
  df <- as.data.frame(matrix(unlist(data), nrow=length(data), byrow=T))
  cat(".")
  df2 <- as.data.frame(matrix(unlist(data2), nrow=length(data2), byrow=T))
  cat(".")

  # Rename the columns
  colnames(df) <- names(data[[1]])
  colnames(df2) <- names(data2[[1]])
  cat(".")

  # Convert the 'date' columns to Date format
  df$date <- lubridate::ymd(df$date)
  df2$date <- lubridate::ymd(df2$date)

  cat(".")

  if (!is.null(type)) {
    df <- df[df$type==type,]
  }
  if (!is.null(date_start) || !is.null(date_end)) {
    # Convert the 'date' column to Date format
    df$date <- lubridate::ymd(df$date)
    df2$date <- lubridate::ymd(df2$date)

    # Apply date filters
    if (!is.null(date_start)) {
      df <- df[df$date >= lubridate::ymd(date_start),]
      df2 <- df2[df2$date >= lubridate::ymd(date_start),]
    }

    if (!is.null(date_end)) {
      df <- df[df$date <= lubridate::ymd(date_end),]
      df2 <- df2[df2$date <= lubridate::ymd(date_end),]
    }
    cat(".")
  }
  cat(".OK!\n")
  return(rbind(df, df2))
}

#' Load a GeoTIFF file and convert it to a matrix
#'
#' @param url The URL of the GeoTIFF file.
#' @return A matrix representation of the GeoTIFF data.
#' @export
load_geotiff <- function(url) {
  m <- as.matrix(0)
  tryCatch(
        #try to do this
        {
          # Download the file to a temporary location
          temp_file <- tempfile(fileext = ".tif")
          download.file(url, temp_file, mode = "wb")

          # Load the GeoTIFF file as a raster
          r <- raster::raster(temp_file)

          # Convert the raster to a matrix
          m <- raster::as.matrix(r)
        },
        #if an error occurs, tell me the error
        error=function(e) {
            m <- as.matrix(0)
        }
    )
    return(m)
}


#' Process a list of rasters and convert them to matrices
#'
#' @param rasters A data frame of rasters, such as the output from query_rasters().
#' @return A list of matrices, one for each raster in the input.
#' @export
download_rasters <- function(rasters) {
  # Get the number of cores
  num_cores <- (parallel::detectCores() - 1)

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

query_variables <- function() {
  # Get the API token
  token <- get_api_token()

  # Base URL for the API
  base_url <- "https://agrodigital.io/api/iot/variables/"

  # Send a GET request to the API
  response <- httr::GET(base_url, add_headers(Authorization = paste0("Token ", token)))

  # Check the status of the response
  if (httr::http_error(response)) {
    print(response)
    stop("An error occurred while trying to query the variables.")
  }

  # Parse the response as JSON
  data <- httr::content(response, as = "parsed")

  # Convert the data to a data frame
  df <- do.call(rbind, lapply(data, function(x) {
    data.frame(
      id = as.numeric(x$id),
      name = as.character(x$name),
      unit = as.character(x$unit),
      stringsAsFactors = FALSE
    )
  }))

  return(df)
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

get_iot_data <- function(station_id) {
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

  # Get variable data
  variables <- query_variables()

  # Join variable data to the measurements dataframe
  df <- merge(df, variables, by.x = "variable", by.y = "id", all.x = TRUE)

  # Rename columns
  names(df)[names(df) == "name"] <- "variable_name"
  names(df)[names(df) == "unit"] <- "variable_unit"

  return(df)
}


