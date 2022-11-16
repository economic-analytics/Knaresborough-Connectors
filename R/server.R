server <- function(input, output, session) {

# UI Rendering --------------------------------------------------------------

  output$dataset <- renderUI({
    selectInput(inputId  = "dataset",
                label    = "Select dataset(s)",
                choices  = edd_dict$desc[edd_dict$id %in% names(edd_datasets)],
                multiple = TRUE
    )
  })

  output$variable_filter <- renderUI({
    checkboxInput(inputId = "variable_filter",
                  label   = "Search only selected datasets?",
                  value   = TRUE
    )
  })

  select_variable_choices <- reactive({
    req(edd_datasets)
    if (input$variable_filter) {
      source_object <- user_datasets()
    } else {
      source_object <- edd_datasets
    }
    lapply(source_object, function(ds) {
      as.list(ds$dimensions$variable$name)
    })
  })

  output$variable <- renderUI({
    value <- isolate(input$variable)
    selectInput(inputId  = "variable",
                label    = "Select variable",
                choices  = select_variable_choices(),
                selected = value,
                multiple = TRUE
    )
  })

  output$dimensions <- renderUI({
    dims_available <- lapply(user_datasets(),
                             function(ds) {
                               names(ds$dimensions)
                             }
    ) |>
      unlist() |>
      unique()

    dims_available <- dims_available[dims_available != "variable"]
    lapply(dims_available, function(i) {
      value <- isolate(input[[i]])
      selectInput(i,
                  paste("Select", stringr::str_replace(i, "_", " ")),
                  choices = lapply(user_datasets(),
                                   function(ds) {
                                     build_input_choices(ds$dimensions[[i]])
                                   }),
                  selected = value,
                  multiple = TRUE
      )
    })
  })

  output$transformations <- renderUI({
    selectInput(inputId = "transformations",
                label   = "Transform data series",
                choices = c("None (data as published)"   = "none",
                            # "Nominal change on previous" = "nominal_change",
                            # "Percent change on previous" = "percent_change",
                            # "Cumulative change"          = "cumulative_change",
                            "Index"                      = "index")
    )
  })

   transformation_date_choices <- reactive({
    # allows selection only of those dates which are available for all of the
    # data currently displayed on the plot
     df <- selected_data_df() |>
      dplyr::group_by(dates$date) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::filter(n == max(n))
    df$`dates$date`
  })

  output$transformation_date <- renderUI({
   req(input$transformations)
     if (input$transformations == "index") {
      value <- isolate(input$transformation_date)
      selectInput(inputId  = "transformation_date",
                  label    = "Select date to index to",
                  choices  = transformation_date_choices(),
                  selected = value
      )
    }
  })

  output$frequency <- renderUI({
    req(input$dataset)
    frequencies <- lapply(user_datasets(), function(ds) ds$data$dates) |>
      dplyr::bind_rows(.id = "dataset") |>
      dplyr::distinct()
    value <- isolate(input$frequency)
    checkboxGroupInput(inputId  = "frequency",
                       label    = "Which frequencies?",
                       choices  = unique(frequencies$freq),
                       selected = c(value, unique(frequencies$freq)[1]),
                       inline   = TRUE
    )
  })

  output$dates <- renderUI({
    req(input$dataset)
    frequencies <- lapply(user_datasets(), function(ds) ds$data$dates) |>
      dplyr::bind_rows(.id = "dataset") |>
      dplyr::distinct()
    min <- min(frequencies$date)
    max <- max(frequencies$date)
    sliderInput("dates", label = "Select time period",
                min = min,
                max = max,
                value = c(min, max)
    )
  })

  output$map_output <- renderUI({
    radioButtons(inputId = "geog_type",
                 label   = "Display data by:",
                 choices = names(boundaries),
                 inline  = TRUE
    )
    #          uiOutput("map_date_select"),
    #          leaflet::leafletOutput("leafletmap")
  })

  # new reactive to handle dimensions in the built df to avoid calculating twice
  # excludes dataset, dates and value as these will always exist in all dataset dfs
  available_dimensions <- reactive({
    names(selected_data_df())[!names(selected_data_df()) %in% c("dataset", "dates", "value")]
  })

  # GLOBAL VARIABLE
  plot_aesthetics <- c("Colour", "Facet", "Linetype", "Shape")

  output$plot_aes <- renderUI({
    # dims <- isolate(available_dimensions())

    lapply(plot_aesthetics, function(aes) {
      value <- isolate(input[[aes]])
      selectInput(aes,
                  aes,
                  choices = c("Dimension" = "",
                              available_dimensions()
                              ),
                  selected = value,
                  multiple = FALSE
      )
    })
  })

  output$y_axis_zero <- renderUI({
    checkboxInput("y_axis_zero", "Force y-axis to include zero")
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("edd_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file)
    }
  )

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("edd_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(jsonlite::flatten(selected_data_df()), file)
    }
  )

  # Reactive Objects --------------------------------------------------------

  # LIST of edd dataset objects in use by the user
  user_datasets <- reactive({
    edd_datasets[edd_dict$id[edd_dict$desc %in% input$dataset]]
  })

  # LIST of edd dataset objects whose $data df has been filtered
  filtered_datasets <- reactive({
    lapply(user_datasets(), function(edd_obj) {
      dims <- names(edd_obj$dimensions)
      data <- edd_obj$data
      for (d in seq_along(dims)) {
        # TODO should we let all data through if input is NULL, or should we let
        # no data through if input is NULL?
        # TODO tidy before deploy ----
        if (1 == 1) { #!is.null(input[[dims[[d]]]])) {
          data <- dplyr::filter(data,
                                .data[[dims[[d]]]] %in%
                                  edd_obj$dimensions[[dims[[d]]]]$code[
                                    edd_obj$dimensions[[dims[[d]]]]$name %in%
                                      input[[dims[[d]]]]]
          )
        }
      }

      # filters for the non-dynamic dimensions (so can be hard-coded)
      data <- dplyr::filter(data,
                            dates$date  >=  input$dates[1],
                            dates$date  <=  input$dates[2],
                            dates$freq %in% input$frequency
      )

      edd_obj$data <- data
      edd_obj
    })
  })


  # Full Data Frame ---------------------------------------------------------

  # creates a single df containing all lookups of user selected data
  selected_data_df <- reactive({
    lapply(filtered_datasets(), function(ds) edd_obj_to_dataframe(ds)) |>
      dplyr::bind_rows(.id = "dataset")
  })

  # TODO needs to have empty values filled with defaults for each dimension

  # TODO pass df through ts_transformations() only if input$transformations
  # isn't selected to "as published". This will move the if statement at
  # line here + 9 to outside ts_transformations()

  ggplot_data <- reactive({
    selected_data_df() |>
      ts_transformations()
  })

  # ts_transformations only works with index - TODO rest need adding
  # can do this through list function - value in selectInput should correspond
  # to name of function in time series transformation list object
  ts_transformations <- function(df) {
    req(input$transformations)
    if (input$transformations == "none") {
      return(df)
    } else if (input$transformations == "index" && !is.null(input$transformation_date)) {
      columns_to_group_by <- names(df)[!names(df) %in% c("dataset", "dates", "value")] # TODO this crops up in a few places - global constant?
      dplyr::group_by(df, dplyr::across(dplyr::all_of(columns_to_group_by))) |>
        ts_transform_df$index(input$transformation_date)
    }
  }

  # map_data should be filtered by reactive values on all dimensions
  # *INCLUDING* date but *EXCEPT* geography - all geog_levels from UI select
  # [geog_type] should be included

  output$map_date_select <- renderUI({
    sliderInput(inputId = "map_date_select",
                label   = "Select date",
                min     = min(data_to_plot()$dates$date),
                max     = max(data_to_plot()$dates$date),
                value   = max(data_to_plot()$dates$date)
    )
  })

  map_pal <- reactive({
    leaflet::colorNumeric("YlOrRd", domain = map_data()$value)
  })

# Plot Logic --------------------------------------------------------------

  manage_plot_group <- function(i) {
    if (length(input[[i]]) > 1) {
      # find first unselected input$aes_*
      for (aes in plot_aesthetics) {
        if (input[[aes]] == "") {
          updateSelectInput(session,
                            aes,
                            selected = i)
          break
        }

        if (input[[aes]] == i) {
          break
        }
      }

    } else {
      # find which input$aes_* contains it and remove it
      for (aes in plot_aesthetics) {
        if (input[[aes]] == i) {
          updateSelectInput(session,
                            aes,
                            selected = "")
          break
        }
      }
    }
  }

  inputs <- lapply(edd_datasets, \(x) {
    names(x$dimensions)
  }) |> unlist() |> unique()

  # Generate observers on the available_dimensions
  lapply(inputs, function(i) {
    observeEvent(input[[i]], {
      # print(input[[i]]) # testing only
      # print(inputs) # testing only
      manage_plot_group(i)
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
    )
  }
  )

# Plot Output -------------------------------------------------------------

  output$dataplot <- renderPlot({
    req(ggplot_data())
    if (nrow(ggplot_data()) < 10000 && nrow(ggplot_data()) > 0) {
      ggplot2::ggplot(ggplot_data(),
                      ggplot2::aes_string(x        = "dates$date",
                                          y        = "value",
                                          colour   = {if (input$Colour == "") NULL else paste0(input$Colour, "$name")},
                                          linetype = {if (input$Linetype == "") NULL else paste0(input$Linetype, "$name")},
                                          shape    = {if (input$Shape == "") NULL else paste0(input$Shape, "$name")}
                      )
      ) +
        ggplot2::geom_line(size = 1) +
        {if (input$Shape != "") ggplot2::geom_point(size = 3)} +
        {if (input$Facet != "") ggplot2::facet_wrap(as.formula(paste0("~ ", input$Facet, "$name")))} +
        ggplot2::labs(x        = NULL,
                      y        = plot_ylab(ggplot_data(), input),
                      title    = "Chart title",
                      subtitle = "Chart subtitle",
                      caption  = plot_caption(input$dataset),
                      colour   = stringr::str_to_sentence(input$Colour),
                      linetype = stringr::str_to_sentence(input$Linetype),
                      shape    = stringr::str_to_sentence(input$Shape)
        ) +
        ggplot2::theme(panel.background   = ggplot2::element_blank(),
                       panel.grid         = ggplot2::element_blank(),
                       panel.grid.major.y = ggplot2::element_line(linetype = "dotted"),
                       legend.position    = "top",
                       axis.line.y.right  = NULL,
                       axis.line          = ggplot2::element_line(),
                       text               = ggplot2::element_text(size = 16)
        ) +
        {if (input$y_axis_zero) ggplot2::ylim(min(0, min(ggplot_data()$value)), NA)}

    }
  })


# Table Output ------------------------------------------------------------

  output$datatable <- DT::renderDT({
    jsonlite::flatten(selected_data_df())
  })


# Map Output --------------------------------------------------------------

  # output$leafletmap <- leaflet::renderLeaflet({
  #   # data <- filter(data_to_plot(),
  #   #                dates$date == input$dates)
  #   # boundaries <- get(input$geog_type, boundaries) # access geoms for input$geog_type
  #   #
  #   # mapdata <- dplyr::inner_join(boundaries,
  #   #                              data,
  #   #                              # by = setNames("geography", paste0(input$geog_type, "18cd"))
  #   #                              by = setNames("geography", names(boundaries)[2])
  #   #                              )
  #   # pal <- colorNumeric("YlOrRd", domain = mapdata$data$value) # needs to be reactive
  #   pal <- map_pal()
  #
  #   leaflet::leaflet() %>%
  #     leaflet::addTiles() %>%
  #     leaflet::addPolygons(data = map_data(),
  #                          fillColor = ~pal(value),
  #                          weight = 1,
  #                          color = "Blue",
  #                          label = map_data()$value)
  # })
} # server
