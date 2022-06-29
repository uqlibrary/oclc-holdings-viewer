# load necessary packages
library(shiny)
library(shinyvalidate)
library(shinycssloaders)
library(htmltools) # to include markdown
library(tmaptools) # for geocoding
library(tmap) # for maps
library(writexl) # for XLSX export
library(DT) # for renderDT()
library(sf) # for spatial data operations
# load helper functions
source("helpers.R")
# TODO: remove when not needed
cities <- readRDS("cities.rds")
# load static map background
data("World")

#### User Interface ####

ui <- fluidPage(

  # Application title
  titlePanel("Get institutions from OCLC number"),

  # Sidebar with controls and stats
  sidebarLayout(
    sidebarPanel(
      p("This tool retrieves a list of OCLC institutions that hold a particular
        publication, makes the table available to download, and generates maps if needed."),
      p("Please do not share this link outside of the UQ Library."),
      h3("1. Get the data"),
      textInput("OCLC",
                "OCLC number"),
      p("Find it on the",
        a("WorldCat website", href = "https://www.worldcat.org", .noWS = "after"),
        ". Try 1125093341 for an example."),
      numericInput("maxRes",
                   "Maximum number of results (multiple of 50)",
                   value = 2000,
                   min = 50, # minimum value is validated in server code
                   step = 50),
      actionButton("go", "Get the data", icon("paper-plane"),
                   style = "background-color: #c2ffad"),
      conditionalPanel( # data sidebar: shown when go button clicked
        condition = "input.go",
        actionButton("reset_button", "Reset the app", icon("undo"),
                     style = "background-color: #ffb3b3"),
        h3("2. Download the data"),
        downloadButton("downloadCSV", "Download CSV"),
        downloadButton("downloadXLSX", "Download XLSX"),
        h3("3. Map the cities"),
        p("Warning: this can be slow for large numbers of locations."),
        radioButtons(
          "map_it",
          label = "Select a type to generate a map:",
          choices = c("Interactive" = "map_it_int",
                      "Static" = "map_it_sta"),
          selected = character(0)
        )
      )
    ),

    # output panel
    mainPanel(
      tabsetPanel(
        id = "tabset",
        type = "tabs",
        tabPanel("Results",
                 conditionalPanel(
                   condition = "!input.go",
                   br(),
                   p('Please input an OCLC number and press "Get the data" in the sidebar.')
                 ),
                 conditionalPanel( # data sidebar: shown when go button clicked
                   condition = "input.go",
                   # stats
                   h3("Statistics"),
                   textOutput("nb_results"),
                   tags$ul(
                     tags$li(textOutput("nb_libs")),
                     tags$li(textOutput("nb_cities")),
                     tags$li(textOutput("nb_countries"))
                   ),
                   tags$small("*'institutions' might include digital content distributors."),
                   # data table
                   h3("Data table"),
                   dataTableOutput("results") %>% withSpinner(color = "#49075e")
                 )
                ),
        tabPanel("Map",
                 value = "maptab",
                 # hint if no type selected
                 conditionalPanel(
                   condition = "!input.map_it",
                   br(),
                   p("Please select a map type in the sidebar")
                 ),
                 # interactive map
                 conditionalPanel(
                   condition = "input.map_it == 'map_it_int'",
                   h3("Locations"),
                   tmapOutput("mapInt", height = 500) %>% withSpinner(color = "#49075e")
                 ),
                 # static map
                 conditionalPanel(
                   condition = "input.map_it == 'map_it_sta'",
                   h3("Locations"),
                   plotOutput("mapSta", height = "700px") %>% withSpinner(color = "#49075e")
                  ),
                 conditionalPanel(
                   condition = "input.map_it",
                   h4("Notes"),
                   textOutput("missing_locs") %>% withSpinner(color = "#49075e", size = 0.5),
                   p("Locations geocoded thanks to the ",
                     a("OpenStreetMap Nominatim API",
                       href = "https://nominatim.org/release-docs/develop/api/Overview/",
                       .noWS = "after"),
                     ".")
                   )
                 ),
        tabPanel("Help",
                 htmltools::includeMarkdown("help.md")
                 )
      )
    )
  )
)

#### Server ####

# Define server logic required to get library holdings from WorldCat
server <- function(input, output, session) {

  #### get data ####

  # get user settings
  OCLC <- eventReactive(input$go, {
    input$OCLC
  })
  maxRes <- eventReactive(input$go, {
    input$maxRes
  # get data from API
  })
  results <- reactive({
    get_all(oclc = OCLC(), limit = maxRes())
  })
  # turn the URLs into short clickable hyperlinks
  results_with_html <- reactive({
    results() %>%
      mutate(self = paste0("<a href='", self, "' target='_blank'>",
                              paste0(substr(self, 1, 20), "..."),
                              "</a>"))
  })
  # render table
  output$results <- DT::renderDT({results_with_html() %>%
                                   datatable(escape = -8, # don't escape the URL HTML
                                             options = list(pageLength = 10))})

  #### download ####

  # make table available for download
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste0(OCLC(), ".csv")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$downloadXLSX <- downloadHandler(
    filename = function() {
      paste0(OCLC(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(results(), file)
    }
  )

  #### stats ####

  output$nb_results <- renderText({
    results() %>% nrow() %>% paste("results, including:")
  })
  small_df <- reactive({
    results() %>%
      distinct(institutionName, country, city) %>%
      mutate(addr = paste(city, country, sep = ", "))
  })
  # number of institutions
  output$nb_libs <- renderText({
    small_df() %>% nrow() %>% paste("unique 'institutions'*")
  })
  # number of countries
  output$nb_countries <- renderText({
    small_df()[["country"]] %>%
      na.omit() %>%
      unique() %>%
      .[nzchar(.)] %>% # remove empty string
      length() %>%
      paste("unique countries")
  })
  locs <- reactive({
    small_df() %>% count(addr)
  })
  # number of cities
  output$nb_cities <- renderText({
    locs() %>% nrow() %>% paste("unique cities")
  })

  #### mapping ####

  # TODO: use OCLC API for locations of libraries
  # (likely quicker and more accurate than Nominatim method)

  # get missing coordinates from Nominatim

  # which locations need coordinates
  need_coords <- eventReactive(input$go, {
    locs() %>%
      anti_join(cities, by = c("addr" = "query"))
  })

  geocoded <- reactive({
    # if built-in dataset doesn't have all required locations, get missing ones
    if (nrow(need_coords()) != 0) {
      need_coords() %>%
        .[["addr"]] %>%
        geocode_OSM(as.sf = TRUE) %>%
        bind_rows(cities) %>%
        right_join(locs(), by = c("query" = "addr"))
    } else {
      # otherwise, only use the built-in dataset
      cities %>%
        right_join(locs(), by = c("query" = "addr"))
    }
  })
  # find out how many cities couldn't be placed on the map
  output$missing_locs <- renderText({
    geocoded() %>%
      filter(is.na(lat)) %>% # could also be "lon", but not "point" as it has "POINT EMPTY"
      nrow() %>%
      paste("Locations that could not be geocoded:", .)
  })

  # send interactive map
  output$mapInt <- bindEvent(
    renderTmap({
      tmap_mode("view")
      tm_shape(geocoded()) +
        tm_dots(size = "n", shape = 21, scale = 1/3, alpha = 0.7, col = "orange")
    }),
    input$map_it == "map_it_int"
  )
  # send static map
  output$mapSta <- bindEvent({
    size_range <- reactive(range(geocoded()$n))
    size_breaks <- reactive(c(size_range()[1],
                                round(mean(size_range())),
                                size_range()[2]) %>%
                                unique())
    renderPlot({
      geocoded_obj <- geocoded()
      st_crs(World) <- 4326
      tmap_mode("plot")
      tm_shape(World) +
        tm_fill("lightgrey") +
        tm_shape(geocoded_obj) +
        tm_bubbles(size = "n", shape = 21, alpha = 0.7, col = "orange",
                   sizes.legend = size_breaks()) +
        tm_layout(legend.frame = TRUE, legend.bg.color = "lightgrey",
                  legend.bg.alpha = 0.5)
    })
  },
    input$map_it == "map_it_sta"
  )

  # reset session action
  observeEvent(input$reset_button, {session$reload()})

  # jump to map tab when type of map selected
  observeEvent(input$map_it, {
    updateTabsetPanel(
      session,
      inputId = "tabset", # which tabset
      selected = "maptab" # which tab
    )
  })

  # validate numeric input for maxRes
  iv <- InputValidator$new()
  iv$add_rule("maxRes", sv_between(50, Inf, message_fmt = "Must be at least {left}."))
  iv$enable()

  # TODO: add export (static and interactive)

}

#### Run app ####
shinyApp(ui = ui, server = server)
