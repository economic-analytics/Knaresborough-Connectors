launch_edd <- function() {
  shinyApp(ui = ui, server = server, enableBookmarking = "url")
}
