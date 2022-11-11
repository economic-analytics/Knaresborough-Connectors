update_edd_dict_from_csv <- function() {
  edd_dict <- readr::read_csv(
    "data-raw/edd_dict.csv",
    col_types = readr::cols(.default = readr::col_character(),
                            status = readr::col_logical(),
                            obj_available = readr::col_logical()))
  usethis::use_data(edd_dict, overwrite = TRUE)
}
