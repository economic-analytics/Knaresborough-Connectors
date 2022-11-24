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
    } else {
      caption <- paste(caption_df$provider, collapse = ", ",
                       paste(caption_df$desc, collapse = ", "))
    }
  } else {
    caption <- "[sources]"
  }

  return(paste0("Source: ", caption, "\nPowered by EDD"))
}

plot_ylab <- function(ggplot_data, input) {
  if (input$transformations == "none") {
    if (exists("ggplot_data$variable$unit")) {
      ylab <- ggplot_data$variable$unit
    } else {
      ylab <- "Value"
    }
  } else {
    ylab <- paste0(stringr::str_to_sentence(input$transformations),
                  " (", date_iso_to_text(input$transformation_date,
                                        input$frequency),
                  " = 100)"
    )
  }
  return(ylab)
}
