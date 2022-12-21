ui <- function(request) {
  navbarPage(
    title = "EDD v0.0.0.9020 ALPHA",
    id = "navbar",
    windowTitle = "EDD: Economic Data Dashboard",

    tabPanel(
      id = "datatool",
      title = "Interactive data tool",

    # Application title
    titlePanel("Knaresborough Economic Data Dashboard"),

      # Sidebar UI
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(id = "datatool_sidebar_tabs",

                      tabPanel(title = "Select by dataset",
                               uiOutput("dataset"),
                               # uiOutput("variable_filter"),
                               # uiOutput("variable"),
                               uiOutput("dimensions"),
                               uiOutput("dates"),
                               uiOutput("frequency"),
                               uiOutput("transformations"),
                               uiOutput("transformation_date")
                      )
          )
        ),

        # Main panel UI
        mainPanel(
          tabsetPanel(id = "datatool_mainpanel_tabs",
                      type = "pills",

                      tabPanel(title = "Chart",
                               sidebarLayout(
                                 sidebarPanel(
                                   p(strong("Chart options")),
                                   tabsetPanel(
                                     tabPanel("Plot aesthetics",
                                              uiOutput("plot_aes"),
                                              uiOutput("y_axis_zero")
                                     )
                                   )
                                 ),
                                 mainPanel(
                                   plotOutput("dataplot"),
                                   downloadButton("download_plot",
                                                  label = "Download as PNG")
                                 ),
                                 position = "right"
                               )
                               # uiOutput("plot_group"),


                               # uiOutput("summarise"),

                      ),

                      tabPanel(title = "Table",
                               DT::DTOutput("datatable"),
                               downloadButton("download_data",
                                              label = "Download as CSV")
                      )
                      # ,
                      #
                      # tabPanel(title = "Map",
                      #          uiOutput("map_output"),
                      #          uiOutput("map_date_select"),
                      #          leaflet::leafletOutput("leafletmap")
                      # )
          )
        )
      )
    ),

    # Menu: about
    tabPanel(
      id = "about",
      title = "About",
      h2("About EDD"),
      p("EDD, the Economic Data Dashboard, is a tool currently under development."),
      p("It is designed to make economic data more useful, particularly at a sub-regional level, to support better analysis and policymaking in local areas."),
      p("EDD will, in its first beta release (v0.0.1 expected Q4 2022), provide interactive access to most ONS datasets, including small area estimates, providing a repository of small-geography economic data in a structured, linked format, with ISO8601 date formats and standard ONS geography codes to support easier access to cleaned and cubed data from multiple sources."),
      p("Later, EDD will include Land Registry housing transacation data, all NOMIS datasets and Companies House data for all active companies in the UK, including an API to access all data across multiple sources."),
      p("The development plan includes both imputation of all economic data to a small-area level (not larger than MSOA) and a quarterly-updated forecasting model for all economies across the UK. Both of these will be on a commercial subscription basis."),
      p("If you're interested in its development, more information is available on its Github page at github.com/economic-analytics/edd."),
      p("For more information, please contact Christian Spence at mail@christianspence.co.uk")
    ),

    tabPanel(
      id = "admin",
      title = "Admin",
      actionButton(inputId = "update_ons",
                   label   = "Update ONS datasets")
    ),

    tabPanel(
      id = "data_catalogue",
      title = "Data catalogue",
      DT::DTOutput("data_catalogue")
    ),

    tabPanel(
      id = "changelog",
      title = "Change log",
      h2("Change log"),
      h3("2022-12-06 v0.0.0.9020"),
      p("Add changelog to app"),
      p("Add Blue Book, Business Investment, Labour Productivity, Quarterly National Accounts, Regional Productivity, Unit Labour Cost & Unit Wage Cost to available ONS datasets"),
      p("Add searchable data catalogue"),
      h3("2022-12-05 v0.0.0.9019"),
      p("Deploy bookmarking/persistent URLs to production"),
      p("UX improvements: add closing 'x' buttons to all dataset, variable and dimension labels"),
      p("Improve dynamic caption logic"),
      p("Removal of redundant code"),
      h3("2022-12-02 v0.0.0.9018"),
      p("Update deployment URL"),
      p("Workbench code for OOP creation, verification and coercion"),
      p("Add {lubridate} to Imports (date functions)"),
      h3("2022-12-01 v0.0.0.9017"),
      p("Add Regional Gross Disposable Household Income to data dictionary and include data in deployed app"),
      p("Improve functionality to convert data to `edd_obj`"),
      h3("2022-11-25 v0.0.0.9016"),
      p("Add initial bookmarking/persistent URL features"),
      h3("2022-11-24 v0.0.0.9015"),
      p("Add NOMIS clamaint count to data dictionary (for MSOA sub sites)"),
      p("Add `data_bres_add_emp_status_options()` which creates FTE and other employment totals"),
      p("Amend `add_hocl_msoa_names()` to use `geography_code` as `code_col` default"),
      p("Improve dynamic caption labels on plot"),
      h3("2022-11-23 v0.0.0.9014"),
      p("Remove non-vectorised `ons_parse_dates()` and implement a vectorised `date_text_to_df()`"),
      p("Add `message()` options to let users know that parsing dates can be slow"),
      p("Update `edd_dict` to add TEST lookup object"),
      p("Add new functions for parsing NOMIS csv files, converting these to `edd_df`, `edd_df` to `edd_obj`, generating dimension totals and adding hocl MSOA names to flat dfs"),
      h3("2022-11-17 v0.0.0.9013"),
      p("`ons_update_datasets()` now has `save_separate_rds = TRUE` (fixes #13)"),
      p("File rename for clarity"),
      p("Comment and naming convention improvements"),
      p("Minor UI tweaks"),
      p("Update deployment URL"),
      h3("2022-11-16 v0.0.0.9012"),
      p("Remove redundant code"),
      p("Add linewrap to facet strips"),
      p("Add `nrow = 2` to legends"),
      p("Add {scales} for removal of scientific notation on y-axis of plot"),
      p("Amends to handle `y_axis_zero` and `scales::label_comma()` simultaneously"),
      p("Chart options UI improvements"),
      h3("2022-11-15 v0.0.0.9011"),
      p("Full rebuild of plot aesthetic mapping tools, logic and UI"),
      p("Improve dynamic y-axis label logic"),
      p("Add user option to force y-axis to zero"),
      h3("2022-11-14 v0.0.0.9010"),
      p("Add dynamic y-axis labelling to plot to handle when data is manually indexed"),
      h3("2022-11-11 v0.0.0.9009"),
      p("Update naming conventions throughout to EDD"),
      p("Add `dir.exists()` check for separate ONS dataset rds file writes"),
      p("Change default plot and csv download file names to remove time (`:` not allowed in filenames)"),
      p("Improvements to global variable search (i.e. without selecting dataset first)"),
      p("Create dynamic data sources and add to plot caption"),
      h3("2022-11-09 v0.0.0.9008"),
      p("Change aesthetic mapping on charts to have facets as second option"),
      p("Force y-axis of plots to be zero if `min(value) > 0`"),
      h3("2022-11-08 v0.0.0.9007"),
      p("Change event listeners on input dimensions to be dynamic"),
      p("Increase chart font sizes"),
      h3("2022-11-07 v0.0.0.9006"),
      p("Amends to ensure `selectInput()` choices always utilise `<optgroup>` by coercing to a list to handle single item lists"),
      p("Add ONS House Price Statistics for Small Areas (MSOA median) (HPSSA2) to data dictionary"),
      p("Add ONS Small Area Population Estimates (SAPE) to data dictionary"),
      h3("2022-10-31 v0.0.0.9005"),
      p("Rebrand from EDDIE to EDD"),
      p("Improve automatic allocation of dimensions to plot aesthetics"),
      p("Fully implement data load from RDS files in `data/datasets`"),
      p("ons_post_processing() removed as nesting additional list - added to issues #19"),
      h3("2022-10-28 v0.0.0.9004"),
      p("Some amendments to structures to allow for forking of main repo to generate a subsite for Knaresborough MSOA economic data"),
      p("Improve file path generation so now non-Windows dependent"),
      p("Change dataset load to read all available RDS files in a single directory rather than from data dictionary"),
      p("UX improved by removing error messages by insertion of `req()` calls"),
      h3("2022-09-12 v0.0.0.9003"),
      p("Reinstate `ons_post_processing()` call"),
      p("Minor changes to server.R on permanent dimensions"),
      p("Brand update to EDDIE"),
      p("RGDHI local file location updated"),
      h3("2022-09-02 v0.0.0.9002"),
      p("Add ONS Regional Gross Disposable Household Income parsing script"),
      p("Add ONS Regional Gross Value Added (local authority districts) parsing script"),
      p("Update ONS Regional Gross Value Added parsing script to deal with ITL regions instead of NUTS"),
      p("Add `utils-df-to-fedobj()` script to improve data processing"),
      p("Add `utils-tidy-excel-headers.R` with `merge_column_names()` function to clean Excel files with merged cell and multiple row headers"),
      p("Improve parsing of text format dates"),
      p("Improve dynamic allocation of dimensions to plot aesthetics"),
      p("Temporarily remove `ons_post_processing()` call to change ONS datasets back to original formats"),
      h3("2022-05-18 v0.0.0.9001"),
      p("Update magrittr pipe (`%>%`) to base R pipe (`|>`)"),
      p("Update Depends to R >= 4.2"),
      h3("2022-01-08 v0.0.0.9000"),
      p("Initial ALPHA commit"),
      p("Add ONS Regional Gross Fixed Capital Formation")
    )
  )
}
