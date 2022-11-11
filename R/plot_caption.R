plot_caption <- function(datasets) {

  caption_df <- edd_dict |>
    dplyr::filter(desc %in% datasets)

  if (length(datasets) == 1) {
    caption <- paste(caption_df$provider, caption_df$desc)
  }

  if (length(datasets) > 1) {
    if (length(unique(caption_df$provider)) == 1) {
      caption <- paste(caption_df$provider,
                       paste(caption_df$desc, collapse = ", "))
    }
  }

  return(paste0("Source: ", caption, "\nPowered by EDD"))
}
