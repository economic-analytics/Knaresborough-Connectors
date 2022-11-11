edd_obj_to_dataframe <- function(edd_obj) {
  # identify all dimensions in the df that's been passed in
  dims <- names(edd_obj$dimensions)
  data <- edd_obj$data
  for (d in seq_along(dims)) {
    dimension <- edd_obj[["dimensions"]][[d]]
    data <- dplyr::left_join(
      data,
      dimension %>% dplyr::select(name, code), # the select restriction is in place until we can dynamically handle $unit, $type, etc.
      by = setNames("code", dims[d]) # evaluates to: by = c("dims[d] = "code")
    ) %>%
      # at this point it handles dim$name and dim$code, but also needs to manage $unit, $type etc. when present
      dplyr::mutate(!!dims[d] := tibble::tibble(code = !!data[[dims[d]]],
                                                name = name)
      ) %>%
      dplyr::select(-name)
  }
  return(data)
  # TODO we need to deal with the NAs in the table for datasets which do not have
  # dimensions that others do. This will happen at the "bind_rows" point
}
