
data_nomis_to_df <- function(csv_path) {

  df <- readr::read_csv(csv_path,
                        col_types = readr::cols(.default = readr::col_character(),
                                                OBS_VALUE = readr::col_number()))

  col_names <- names(df)
  variables <- stringr::str_remove(col_names,
                                   "_(NAME|CODE|TYPE|TYPECODE|SORTORDER)$") |>
    unique()

  dimensions <- variables[!variables %in% c("OBS_STATUS", "OBS_CONF", "URN", "RECORD_OFFSET", "RECORD_COUNT")]

  var_suffixes_to_drop <- c("TYPECODE", "SORTORDER")

  out_df <- dplyr::select(df, dplyr::starts_with(variables) & !dplyr::ends_with(var_suffixes_to_drop), OBS_VALUE) |>
    dplyr::select(dplyr::ends_with(c("NAME", "CODE", "TYPE")), OBS_VALUE) |>
    dplyr::select(dplyr::starts_with(dimensions), OBS_VALUE) |>
    dplyr::select(-c(DATE_CODE, DATE_TYPE)) |>
    dplyr::select(-MEASURES_NAME) |>
    dplyr::rename(DATE = DATE_NAME) |>
    dplyr::rename(VALUE = OBS_VALUE)

  # Nomis includes industry code inside industry name, so this can be dropped if industry exists as a dimension

  if ("INDUSTRY" %in% variables) {
    out_df <- out_df |>
      tidyr::separate(col = INDUSTRY_NAME, sep = " : ",
                      into = c("INDUSTRY_CODE", "INDUSTRY_NAME"))
  }

  # BRES TABLES ----
  # Nomis BRES tables contain an EMPLOYMENT_STATUS dimension that can contain Employment, Employees, Full-time employees, Part-time employees.
  # Employment is the sum of employees (which is the sum of FT and PT) and "other", self-employed, partners, etc.
  # If EMPLOYMENT_STATUS is in the dataset, then this code separates these out, and also calculates an FTE variable which is full-time + (0.5 * part-time) plus other

  # if ("EMPLOYMENT_STATUS" %in% variables) {
  #   # identify unique values of EMPLOYMENT_STATUS_NAME
  #   emp_statuses <- unique(out_df$EMPLOYMENT_STATUS_NAME)
  #
  #   emp_status_cols <- names(out_df)[grepl("^EMPLOYMENT_STATUS", names(out_df))]
  #
  #   temp_col_names <- paste(out_df[emp_status_cols[1]], out_df[emp_status_cols[2]], out_df[emp_status_cols[3]]) |> unique()
  #
  #   out_df <- out_df |>
  #     tidyr::pivot_wider(names_from = dplyr::starts_with("EMPLOYMENT_STATUS"),
  #                        values_from = OBS_VALUE) |>
  #     # dplyr::mutate(`Other employment` = Employment - (`Full-time employees` + `Part-time employees`)) |>
  #     #
  #     dplyr::mutate(FTE = `Full-time employees` + (0.5 * `Part-time employees`) + `Other employment`) |>
  #     tidyr::pivot_longer(cols = c(dplyr::starts_with(emp_statuses), `Other employment`, FTE), names_to = "EMPLOYMENT_STATUS_NAME", names_sep = "_", values_to = "OBS_VALUE")
  # }

  names(out_df) <- sub("MEASURE_", "VARIABLE_", names(out_df))
  names(out_df) <- tolower(names(out_df))

  return(out_df)
}

############################################


#
# variables <- names(bres)[!names(bres) %in% c("date", "obs_value")]
# # removes everything including and after the last underscore
# dimensions <- sub("_[^_]*$", "", variables) |> unique()
#
# # keeps only that after the last underscore
# "[^_]*$"

df_to_edd_df <- function(df) {

  # TODO add test for standardised column names

  # remove column names that are fixed
  variables <- names(df)[!names(df) %in% c("date", "value")]
  # strip out everything after and including the last underscore
  dimensions <- sub("_[^_]*$", "", variables) |> unique()


  out_df <- list()
  out_df$dates <- date_text_to_df(df$date)

  for (dim in dimensions) {
    out_df[[dim]] <- dplyr::select(df, dplyr::starts_with(dim)) |>
      dplyr::rename_with(~ stringr::str_remove(.x, paste0(dim, "_")))
  }

  out_df$value <- df$value

  out_df <- tibble::as_tibble(out_df)

  # TODO need to add class of edd_df
  return(out_df)

}

edd_df_to_edd_obj <- function(edd_df) {

  # TODO add test for class of edd_df
  dims <- names(edd_df)[!names(edd_df) %in% c("dates", "value")]

  data <- tibble::tibble(
    dates = edd_df$dates
  )

  for (d in dims) {
    data[[d]] <- edd_df[[d]][["code"]]
  }

  data$value <- edd_df$value

  dimensions <- list()

  for (d in dims) {
    dimensions[[d]] <- edd_df[[d]] |> dplyr::distinct()
  }

  obj <- list(data = data,
              dimensions = dimensions)

  return(obj)


}

generate_dimension_total <- function(df, dimension,
                                     dimension_plural = dimension) {

  vars_to_group_by <- names(df)[!grepl(paste0("^", dimension), names(df)) & names(df) != "value"]

  out_df <- df |>
    dplyr::group_by(dplyr::across(vars_to_group_by)) |>
    dplyr::summarise(value = sum(value))

  out_df[paste(dimension, "code", sep = "_")] <- "All"
  out_df[paste(dimension, "name", sep = "_")] <- paste("All", dimension_plural)

  dplyr::bind_rows(df, out_df)
}

add_hocl_msoa_names <- function(df, code_col = "geography_code") {

  if (!file.exists("data-raw/MSOA-Names-Latest.csv")) {
    file_url <- "https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-Latest.csv"
    download.file(file_url, file.path("data-raw", basename(file_url)))
  }

  hocl_msoa_names <- readr::read_csv("data-raw/MSOA-Names-Latest.csv") |>
    dplyr::select(msoa11cd, msoa11hclnm)

  out_df <- merge(df, hocl_msoa_names,
        by.x = code_col, by.y = "msoa11cd", all.x = TRUE) |>
    dplyr::rename(geography_name.ONS = geography_name,
                  geography_name = msoa11hclnm)

  return(tibble::as_tibble(out_df))
}

data_bres_add_emp_status_options <- function(df) {
  pivot_cols <- names(df)[grepl("^employment_status", names(df))]

  df2 <- df |>
    tidyr::pivot_wider(names_from = dplyr::all_of(pivot_cols),
                       values_from = value) |>
    dplyr::mutate(`Other employment_O_Employment` = Employment_4_Employment - Employees_1_Employment,
                  `FTE_F_Employment` = `Other employment_O_Employment` + `Full-time employees_2_Employment` + (0.5 * `Part-time employees_3_Employment`))

  out_df <- df2 |>
    tidyr::pivot_longer(cols = names(df2)[!names(df2) %in% names(df)],
                        names_to = pivot_cols, names_sep = "_")

  return(out_df)
}
