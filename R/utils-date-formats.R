# TODO all functions here need editing and unifying
# TODO need to handle half years, too (e.g. 2020H1)


date_iso_to_text <- function(date, frequency = c("a", "q", "m"), abbr_month = TRUE) {
  if (frequency == "a") {
    return(format.Date(date, "%Y"))
  }

  if (frequency == "q") {
    return(paste(paste0("Q", date_iso_to_quarter(date)),
                 format.Date(date, "%Y")
                 )
    )
  }

  if (frequency == "m") {
    if (abbr_month) {
      month_name <- month.abb[as.integer(format.Date(date, "%m"))]
    } else {
      month_name <- month.name[as.integer(format.Date(date, "%m"))]
    }
    return(paste(month_name, format.Date(date, "%Y"))
    )
  }
}

date_iso_to_quarter <- function(date) {
  return(ceiling(as.integer(format.Date(date, "%m")) / 3))
}

date_text_to_iso <- function(date_as_text, frequency = NULL) {

  #TODO move ons_parse_dates here and refine ----
  ons_parse_dates(date_as_text)
}

date_text_to_df <- function(dates) {
  message("Parsing date formats (can be slow) ...")
  # if the date is four digits only, i.e. a year and annual
  out_df <- purrr::map_df(dates, function(d) {
    if (grepl("^[0-9]{4}$", d)) {
      date <- readr::parse_date(d, format = "%Y")
      freq <- "a"
    } else if (grepl("Q", d)) { # if the date contains a "Q", i.e. quarterly
      date <- lubridate::yq(d)
      freq <- "q"
    } else if (grepl("[0-9]{4}$", d)) { # if contains four digits at the end
      date <- lubridate::my(d)
      freq <- "m"
    } else if (grepl("^[0-9]{4}", d)) { # if it contains four digits at the beginning
      date <- lubridate::ymd(d, truncated = 1)
      freq <- "m"
    } else {
      date <- as.Date(NA) # if none of the above parse, return an NA to highlight the problem
      freq <- NA_character_
    }
    df <-  tibble::tibble(date = date,
                          freq = freq)
  })
  message("Done.")
  return(out_df)
}

# DEPRECATED

# ons_parse_dates <- function(dates,
#                             format = c("df", "vector"),
#                             frequency = FALSE) {
#   format <- match.arg(format)
#
#   if (grepl("^[0-9]{4}$", dates)) { #if 4 digit number
#     date <- readr::parse_date(dates, format = "%Y")
#     freq <- "a"
#   } else if (grepl("Q", dates)) { #if contains 'Q'
#     date <- lubridate::yq(dates)
#     freq <- "q"
#   } else if (grepl("[0-9]{4}$", dates)) {
#     date <- lubridate::dmy(dates, truncated = 1)
#     freq <- "m"
#   } else if (grepl("^[0-9]{4}", dates)) {
#     date <- lubridate::ymd(dates, truncated = 1)
#     freq <- "m"
#   } else {
#     date <- as.Date(NA)
#     freq <- NA_character_
#   }
#
#   if (frequency && format == "df") {
#     df <- tibble::tibble(date = date, freq = freq)
#   } else if (format == "df" && !frequency) {
#     df <- tibble::tibble(date = date)
#   } else if (format == "vector") {
#     df <- date
#   }
#   return(df)
# }
