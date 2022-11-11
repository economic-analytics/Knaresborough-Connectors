# This file contains all of the functions required to download all ONS datasets
# within the edd_dict and process them into the edd_obj format with data,
# variables and meta information in separate lists within lists for each
# time series object


# Master function (for export) --------------------------------------------

ons_update_datasets <- function(save_separate_rds = FALSE, ...) {
  # problems with VROOM_CONNECTION_SIZE means we have to set this as the
  # download is large and overflows the connection buffer
  Sys.setenv("VROOM_CONNECTION_SIZE" = "500000")

  # conditions within fedo_dict to update on
  to_update <- edd_dict %>%
    dplyr::filter(type     == "dataset",
                  provider == "ONS",
                  status   == TRUE)

  # TODO need to add condition for last_updated but need to implement the
  # writing back of the meta data to the fedo_dict first

  datasets <- lapply(to_update$url,
                     function(url) {
                       ons_download_dataset(url, ...)
                     })
  names(datasets) <- to_update$id

  processed <- lapply(datasets, function(x) ons_process_dataset(x))

  # # ONS POST PROCESSING - STILL TESTING
  # processed <- lapply(seq_along(processed), function(i, ds_name) {
  #   if (ds_name[[i]] %in% names(ons_post_processing)) {
  #     ppobj <- ons_post_processing[[ds_name[[i]]]](processed[[i]])
  #   } else {
  #     processed[[i]]
  #   }
  # },
  # ds_name = names(processed)
  # ) %>% setNames(names(processed))

  # TODO see issue #1 - add test for existence of /data
  saveRDS(processed, file.path("data", "ons_datasets.rds"))

  # write separate .rds files for each dataset
  if (save_separate_rds) {
    for (i in seq_along(processed)) {
      message("Writing ", names(processed)[i], ".rds ...")
      saveRDS(processed[[i]],
              file.path("data", "datasets",
                        paste0(names(processed)[i], ".rds"))
      )
      message("Done.")
    }
  }
  message("data/ons_datasets.rds successfully updated")
}


# Download data (not for export) ------------------------------------------

ons_download_dataset <- function(url, save_csv = FALSE) {

  # if we want to keep a copy of the original ONS .csv
  if (save_csv) {
  # TODO test for existence of /data-raw and create if needed
    destfile = paste0("data-raw/", basename(url))
    download.file(url = url, destfile = destfile)
    url <- destfile
  }
  # TODO need to avoid downloading a file above in if(save_csv)
  # and then doing it again below

  # read, ignore column names, make everything strings
  dataset <- readr::read_csv(url,
                             col_names = FALSE,
                             col_types = readr::cols(
                               .default = readr::col_character()
                             )
  )
  return(dataset)
}


# Process raw df (not for export) -----------------------------------------

# this function parses the downloaded csv files and puts them into the fea
# data shape
ons_process_dataset <- function(dataset, new_format = FALSE) {
  if (new_format) {
    variable <- dataset[1:4, ]
    variable <- tibble::as_tibble(variable)
    variable <- t(variable)
    variable <- tibble::as_tibble(variable)
    names(variable) <- as.character(variable[1, ])
    names(variable) <- c("name", "code", "preunit", "unit")
    variable <- variable[-1, ]

    meta <- dataset[5:7, ]
    meta <- tibble::as_tibble(meta)
    meta <- t(meta)
    meta <- tibble::as_tibble(meta)
    names(meta) <- as.character(meta[1, ])
    meta <- meta[-1, ]
    meta <- dplyr::distinct(na.omit(meta))

    names(dataset) <- c("date", as.character(dataset[2, -1]))
    dataset <- dataset[-(1:7), ]
    dataset$date <- purrr::map_df(dataset$date, ons_parse_dates, frequency = TRUE)
    dataset <- tidyr::pivot_longer(dataset, cols = -date, names_to = "variable")
    dataset$value <- as.numeric(dataset$value)

    x <- dplyr::left_join(dataset, variable, by = c("variable" = "code"))
    var_df <- x %>%
      dplyr::select(-date) %>%
      dplyr::rename(code = variable)

    dataset <- tibble::tibble(dates = dataset$date,
                              variable = var_df)

    dataset <- dplyr::filter(dataset, !is.na(variable$value))

    return(dataset)
  } else {
    # separate variable and meta parts of dataset
    variable <- dataset[1:4, ]
    meta  <- dataset[5:7, ]

    #rename data variable and drop variable and meta rows
    names(dataset) <- c("dates", dataset[2, -1])
    data  <- dataset[-(1:7), ]

    # prepare data
    data$dates <- purrr::map_df(data$dates, ons_parse_dates, frequency = TRUE)
    data <- tidyr::pivot_longer(data, -dates, names_to = "variable", values_to = "value")
    data$value <- as.numeric(data$value)
    data <- dplyr::filter(data, !is.na(value))

    # prepare meta
    meta <- t(meta)
    meta <- tibble::as_tibble(meta)
    names(meta) <- meta[1, ]
    meta <- dplyr::rename(meta,
                          last_update = `Release Date`,
                          next_update = `Next release`,
                          notes       = `Important Notes`
    )
    meta <- meta[-1, ]
    meta <- unique(meta)

    last_update <- ons_parse_dates(unique(meta$last_update))
    next_update <- ons_parse_dates(unique(meta$next_update))
    notes <- unique(na.omit(meta$notes))

    meta <- tibble::tibble(last_update = last_update,
                           next_update = next_update,
                           notes = notes)

    # prepare variable df
    variable <- t(variable)
    variable <- tibble::as_tibble(variable)
    names(variable) <- tolower(variable[1, ])
    variable <- variable[-1, ]
    variable <- dplyr::select(variable,
                              code = cdid,
                              name = title,
                              unit,
                              preunit)

    # removing from code as the data df doesn't contain a geography column
    # geography <- tibble::tibble(code = "UK", name = "UK")
    dimensions <- list(variable  = variable) #,
    # geography = geography)

    # prepare final object
    processed <- list(data = data,
                      dimensions = dimensions,
                      meta = meta)

    # readr::write_rds(ons_datasets, "../../Data/ONS/ons_datasets.rds")

    # TODO write back meta to fedo_dict ####


    # TODO write rds objects for each dataset ####
    # TODO write rds object for list of datasets? only updated ones? ####
    # TODO write to sql for each object ####

    return(processed)
  }
}



# this function takes dates as strings and converts them to date objects
# the frequency argument allows the return of a column marked "a", "m" or "q"
# to denote the frequency of the data for each date.
#
# NB IT ALWAYS RETURNS A DATA FRAME. FEA OBJECTS MUST REFERENCE dates$date
# AND dates$freq TO DEAL WITH THE DATA FRAME IN THE DATA FRAME. THIS WILL ALLOW
# THE ADDITION OF ADDITIONAL FIELDS IF REQUIRED, E.G. dates$text FOR A MORE
# HUMAN-READABLE VERSION OF DATES. THIS IS NOT YET IMPLEMENTED.
#
# NB this function is NOT VECTORISED for easier code writing, so should only
# be called through a purrr::map_df function to iterate through individual values
#
# TODO there should probably be a test to see if any of the dates processed
# return an NA (i.e. not parseable) and, if so, print a warning message
ons_parse_dates <- function(dates, frequency = FALSE) {
  # if the date is four digits only, i.e. a year and annual
  if (grepl("^[0-9]{4}$", dates)) {
    date <- readr::parse_date(dates, format = "%Y")
    freq <- "a"
  } else if (grepl("Q", dates)) { # if the date contains a "Q", i.e. quarterly
    date <- lubridate::yq(dates)
    freq <- "q"
  } else if (grepl("[0-9]{4}$", dates)) { # if contains four digits at the end
    date <- lubridate::dmy(dates, truncated = 1)
    freq <- "m"
  } else if (grepl("^[0-9]{4}", dates)) { # if it contains four digits at the beginning
    date <- lubridate::ymd(dates, truncated = 1)
    freq <- "m"
  } else {
    date <- as.Date(NA) # if none of the above parse, return an NA to highlight the problem
    freq <- NA_character_
  }
  if (frequency) {
    # return a two-column data frame
    df <- tibble::tibble(date = date, freq = freq)
  } else {
    # return a one-column data frame
    df <- tibble::tibble(date = date)
  }
    # WHEN NA CHECKER TO BE IMPLEMENTED IT SHOULD GO HERE AND CHECK THE df$date
    # VARIABLE FOR PRESENCE OF NAs
    return(df)
}

