show_all_variables <- function() {
  lapply(edd_datasets, function(x) {
    tibble::tibble(variable = x$dimension$variable$name)
  }) |>
    dplyr::bind_rows(.id = "dataset")
}
