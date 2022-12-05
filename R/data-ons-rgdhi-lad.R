rgdhi_lad <- function() {
  rgdhi_lad_url <- "https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/datasets/regionalgrossdisposablehouseholdincomelocalauthoritiesbyitl1region"

  urls <- rvest::read_html(rgdhi_lad_url) |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")

  files <- urls[grepl(".xls", urls)]
  file_urls <- paste0("http://ons.gov.uk", files) # http not https

  all_data <- lapply(file_urls[1:12], function(file) {
    local_file_path <- paste0("~/Data/ONS/Regional Accounts/GDHI/",
                              sub("regionalgrossdisposablehouseholdincome",
                                  "rgdhi", basename(substring(file, 50))))
    download.file(file, destfile = local_file_path, mode = "wb")
    sheets <- readxl::excel_sheets(local_file_path)
    sheets <- sheets[-(1:2)]
    data <- lapply(sheets, function(sht) {
      readxl::read_excel(local_file_path, sheet = sht,
                         skip = 1, na = c("-"))
    }) |>
      setNames(c("GDHI current",
                 "GDHI per capita current",
                 "GDHI per capita index",
                 "GDHI annual growth",
                 "GDHI per capita annual growth",
                 "GDHI components current",
                 "GDHI per capita components",
                 "GDHI per capita components index",
                 "GDHI components growth",
                 "GDHI per capita components growth")
      )

    return(data)
  })

  final <- lapply(all_data, function(x) {
    lapply(x, function(sht) {
      if (grepl("Transaction", names(sht)[5])) {
        c = 1:5
      } else {
        c = 1:3
      }

      dplyr::filter(sht, !is.na(`LAD code`)) |>
        tidyr::pivot_longer(cols = -(c), names_to = "date")
    }) |>
      dplyr::bind_rows(.id = "variable")
  }) |>
    dplyr::bind_rows() |>
    dplyr::select(date,
                  geography_code = `LAD code`,
                  geography_name = `Region name`,
                  variable_code = variable,
                  variable_name = variable,
                  transaction_code = `Transaction code`,
                  transaction_name = Transaction,
                  value) |>
    dplyr::mutate(date = stringr::str_sub(date, 1, 4)) |>
    tidyr::replace_na(list(transaction_name = "Total")) |>
    dplyr::mutate(transaction_code = dplyr::coalesce(transaction_code,
                                                     transaction_name)) |>
    df_to_edd_df() |>
    edd_df_to_edd_obj()

    saveRDS(final, "data/datasets/RGDHI.rds")
    return(final)
}
