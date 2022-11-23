ons_parse_dates <- function(dates) {
  # if the date is four digits only, i.e. a year and annual
  purrr::map_df(dates, function(d) {
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
    df = tibble::tibble(date = date,
                        freq = freq)
  })
}
