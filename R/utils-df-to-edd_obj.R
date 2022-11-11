# This converts a (appropriately defined) df into a edd_obj
# DF should contain a date column and a value column as a minimum
# Dimensions should be labelled as appropriate, checking with edd_dict
# for naming conventions. If dimensions have multiple columns for codes, names,
# types, etc., they should be defined as [dimension]_[extra], e.g.
# geography_code, geography_name, etc.

# df_to_edd_obj <- function(df) {
#   # error handling
#   if (!is.data.frame(df)) {
#     stop("Object `df` is not a data frame")
#   }
#
#   col_names <- tolower(names(df))
#
#   if (!"date" %in% col_names) {
#     stop("No date variable detected")
#   }
#
#   if (!"value" %in% col_names) {
#     stop("No value variable detected")
#   }
#
#   # remove standardised endings
#   col_names2 <- stringr::str_remove(col_names, "_code|_name|_type|_unit|_preunit") |> unique()
#
#   #
#   cat("Dimensions have been identified as", col_names2)
#
#   df <- dplyr::mutate(df, date = ons_parse_dates(date))
# }

# testing -----------------------------------------------------------------

df_to_edd_obj <- function(df) {
  # TODO insert edd_obj verification here

  # build data df
  data <- df |>
    dplyr::mutate(dates = purrr::map_df(date, ons_parse_dates, frequency = TRUE)) |>
    dplyr::select(dates, dplyr::ends_with("_code"), value) |>
    dplyr::rename_with(~ stringr::str_remove(.x, "_code"))


  # build dimensions dfs
  dims <- names(df)[!names(df) %in% c("date", "value")] |>
    # TODO this list of suffixes should be controlled dynamically
    stringr::str_remove("_code|_name|_type|_unit|_preunit") |>
    unique()

  dimensions <- lapply(dims, function(d) {
    assign(d, df |>
             dplyr::select(dplyr::starts_with(d)) |>
             dplyr::rename_with(~ stringr::str_remove(.x, paste0(d, "_"))) |>
             dplyr::distinct())
  }) |>
    setNames(dims)

  # build meta df
  meta <- tibble::tibble()

  # combine to list
  edd_obj <- list()
  edd_obj$data <- data
  edd_obj$dimensions <- dimensions
  edd_obj$meta <- meta

  # TODO insert final validation checks
  # TODO insert checking for metadata if empty

  return(edd_obj)
}
