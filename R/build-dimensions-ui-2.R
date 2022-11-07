# TODO We need to think about whether this should be driven from the plot_data
# object rather than from the dimensions list

# This interrogates each dimension available in the datasets chosen by the user
# and, if there is more than one type (e.g. industry section, division, etc.,
# or geography NUTS1, NUTS2, etc.) bundles them into a list so that the select
# input marks them as clearly being of separate types
build_input_choices <- function(dimension) {
  if ("typex" %in% names(dimension)) {
    types <- unique(dimension$type)
    if (length(types) > 1) {
      input_choices <- list()
      for (i in seq_along(types)) {
        input_choices[[i]] <- dimension$name[dimension$type == types[i]]
      }
      names(input_choices) <- types
      return(input_choices)
    } else {
      return(dimension$name)
    }
  } else {
    return(as.list(dimension$name))
  }
}
