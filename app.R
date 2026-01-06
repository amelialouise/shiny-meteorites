library(shiny)
library(mapgl)
library(plotly)
library(DT)
library(dplyr)
library(duckdb)
library(DBI)
library(httpuv)
library(bslib)

# ============================================================================
# Configuration
# ============================================================================

DB_PATH <- "data/meteorites_spatial.duckdb"
TILE_SERVER_PORT <- 8082
options(sass.cache = FALSE)

# ============================================================================
# Tile Server Functions
# ============================================================================

parse_tile_path <- function(path) {
  pattern <- "^/tiles/(\\d+)/(\\d+)/(\\d+)\\.pbf$"
  matches <- regmatches(path, regexec(pattern, path))[[1]]

  if (length(matches) == 4) {
    list(
      z = as.integer(matches[2]),
      x = as.integer(matches[3]),
      y = as.integer(matches[4])
    )
  } else {
    NULL
  }
}

tile_to_bbox <- function(z, x, y) {
  n <- 2^z
  min_lon <- x / n * 360 - 180
  max_lon <- (x + 1) / n * 360 - 180

  lat_rad_max <- atan(sinh(pi * (1 - 2 * y / n)))
  max_lat <- lat_rad_max * 180 / pi

  lat_rad_min <- atan(sinh(pi * (1 - 2 * (y + 1) / n)))
  min_lat <- lat_rad_min * 180 / pi

  c(min_lon = min_lon, min_lat = min_lat, max_lon = max_lon, max_lat = max_lat)
}

generate_meteorite_tile <- function(con, z, x, y, filters) {
  bbox <- tile_to_bbox(z, x, y)

  where_conditions <- c(
    sprintf("year BETWEEN %d AND %d", filters$year_min, filters$year_max),
    "mass_tons > 0"
  )

  if (!is.null(filters$size_filter) && filters$size_filter != "all") {
    where_conditions <- c(
      where_conditions,
      sprintf("size_category = '%s'", filters$size_filter)
    )
  }

  if (!is.null(filters$era_filter) && filters$era_filter != "all") {
    where_conditions <- c(
      where_conditions,
      sprintf("era = '%s'", filters$era_filter)
    )
  }

  if (!is.null(filters$name_search) && filters$name_search != "") {
    where_conditions <- c(
      where_conditions,
      sprintf("name ILIKE '%%%s%%'", filters$name_search)
    )
  }

  where_clause <- paste(where_conditions, collapse = " AND ")

  query <- sprintf(
    "
  SELECT ST_AsMVT(tile, 'meteorites') as mvt
  FROM (
    SELECT
      ST_AsMVTGeom(
        ST_Transform(geom, 'EPSG:4326', 'EPSG:3857', TRUE),
        ST_Extent(ST_TileEnvelope(%d, %d, %d))
      ) AS geometry,
      name,
      catalog_id,
      reclass,
      nametype,
      mass_tons,
      year,
      era,
      size_category,
      lpi_entry,
      CONCAT_WS(' | ',
          name,
          CASE 
            WHEN mass_tons * 1e9 < 1e3 THEN CONCAT('Mass: ', CAST(round(mass_tons*1e9,2) AS VARCHAR), ' mg') 
            WHEN mass_tons * 1e6 < 1e3 THEN CONCAT('Mass: ', CAST(round(mass_tons*1e6, 1) as VARCHAR), ' g')
            WHEN mass_tons * 1e3 < 1e3 THEN CONCAT('Mass: ', CAST(round(mass_tons*1e3, 1) AS VARCHAR), ' kg')
            WHEN mass_tons >= 1 THEN CONCAT('Mass: ', CAST(round(mass_tons, 1) AS VARCHAR), ' tons')
            END,
          CONCAT('\nYear: ', CAST(year AS INTEGER))
          ) as info
    FROM meteorites
    WHERE %s
      AND lon BETWEEN %f AND %f
      AND lat BETWEEN %f AND %f
  ) AS tile
  WHERE geometry IS NOT NULL
",
    z,
    x,
    y,
    where_clause,
    bbox["min_lon"],
    bbox["max_lon"],
    bbox["min_lat"],
    bbox["max_lat"]
  )

  tryCatch(
    {
      result <- dbGetQuery(con, query)
      if (nrow(result) > 0 && !is.null(result$mvt[[1]])) {
        result$mvt[[1]]
      } else {
        raw(0)
      }
    },
    error = function(e) {
      message("Tile error: ", e$message)
      raw(0)
    }
  )
}

# ============================================================================
# Connect to Database
# ============================================================================

message("Connecting to DuckDB: ", DB_PATH)
con <- dbConnect(duckdb(), DB_PATH, read_only = TRUE)
dbExecute(con, "INSTALL spatial;")
dbExecute(con, "LOAD spatial;")

metadata <- dbGetQuery(
  con,
  "
  SELECT 
    MIN(year) as min_year,
    MAX(year) as max_year,
    COUNT(*) as total_count
  FROM meteorites
"
)

size_categories <- c(
  "All" = "all",
  "Tiny (< 1kg)" = "Tiny (< 1kg)",
  "Small (1-5kg)" = "Small (1-5kg)",
  "Medium (5-20kg)" = "Medium (5-20kg)",
  "Large (20kg-5t)" = "Large (20kg-5t)",
  "Massive (> 5t)" = "Massive (> 5t)"
)

# ============================================================================
# UI
# ============================================================================

ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#667eea"
  ),
  padding = 0,

  tags$head(
    tags$style(HTML(
      "
      .stats-panel {
        background: white;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }
      .filter-section {
        margin-bottom: 15px;
      }
      .filter-section h5 {
        margin-bottom: 10px;
        color: #667eea;
        font-size: 1.1em;
      }
    "
    ))
  ),

  # Full-screen map
  maplibreOutput("map", height = "100%"),

  # Floating sidebar
  absolutePanel(
    top = 20,
    right = 20,
    width = 320,
    class = "card shadow",
    style = "background: white; border-radius: 8px; padding: 20px; max-height: 90vh; overflow-y: auto;",

    h4("🌌 Meteorite Explorer", class = "mb-3"),

    div(
      class = "filter-section",
      h5("🔍 Filters"),

      sliderInput(
        "year_range",
        "Year:",
        sep = "",
        min = metadata$min_year,
        max = metadata$max_year,
        value = c(metadata$min_year, metadata$max_year),
        step = 10
      ),

      textInput("name_search", "Name:", value = "", placeholder = "Search..."),

      selectInput(
        "size_filter",
        "Size:",
        choices = size_categories,
        selected = "Large (20kg-5t)"
      ),

      selectInput(
        "era_filter",
        "Era:",
        choices = c(
          "All" = "all",
          "Ancient (pre-1800)" = "Ancient (pre-1800)",
          "Historical (1800-1899)" = "Historical (1800-1899)",
          "Early Modern (1900-1949)" = "Early Modern (1900-1949)",
          "Late Modern (1950-1999)" = "Late Modern (1950-1999)",
          "Recent (2000+)" = "Recent (2000+)"
        ),
        selected = "all"
      ),

      actionButton(
        "apply_filters",
        "Apply Filters",
        class = "btn-primary w-100 mb-2"
      ),
      actionButton(
        "reset_filters",
        "Reset",
        class = "btn-outline-secondary w-100"
      )
    ),

    hr(),

    div(
      class = "stats-panel",
      h5("📊 Statistics"),
      verbatimTextOutput("quick_stats", placeholder = TRUE)
    ),

    hr(),

    div(
      class = "stats-panel",
      h5("📊 Mass Distribution"),
      plotlyOutput("mass_histogram", height = "180px")
    ),

    hr(),

    div(
      class = "stats-panel",
      h5("📖 About"),
      tags$p(
        style = "font-size: 0.9em; margin-bottom: 8px;",
        "Data from ",
        tags$a(
          "NASA's Meteorite Database",
          href = "https://data.nasa.gov/dataset/meteorite-landings",
          target = "_blank"
        )
      ),
      tags$p(
        style = "font-size: 0.9em; margin: 0;",
        "Total: ",
        format(metadata$total_count, big.mark = ","),
        " meteorites"
      )
    ),

    hr(),

    actionButton(
      "show_timeline",
      "📈 View Timeline",
      class = "btn-outline-primary w-100 mb-2"
    ),
    actionButton(
      "show_table",
      "📋 View Data Table",
      class = "btn-outline-primary w-100"
    )
  )
)

# ============================================================================
# Server
# ============================================================================

server <- function(input, output, session) {
  # Helper function to format mass
  format_mass <- function(mass_tons) {
    mass_mg <- mass_tons * 1e9
    mass_g <- mass_tons * 1e6
    mass_kg <- mass_tons * 1e3
    ifelse(
      mass_mg < 1e3,
      paste0(round(mass_mg, 1), " mg"),
      ifelse(
        mass_g < 1e3,
        paste0(round(mass_g, 1), " g"),
        ifelse(
          mass_kg < 1e3,
          paste0(round(mass_kg, 1), " kg"),
          paste0(round(mass_tons, 1), " tons")
        )
      )
    )
  }

  # Start tile server
  tile_server <- NULL

  observe({
    if (is.null(tile_server)) {
      tile_app <- list(
        call = function(req) {
          path <- req$PATH_INFO

          if (req$REQUEST_METHOD == "OPTIONS") {
            return(list(
              status = 200L,
              headers = list(
                "Access-Control-Allow-Origin" = "*",
                "Access-Control-Allow-Methods" = "GET, OPTIONS"
              ),
              body = ""
            ))
          }

          coords <- parse_tile_path(path)

          if (!is.null(coords)) {
            message(sprintf(
              "Generating tile: z=%d x=%d y=%d",
              coords$z,
              coords$x,
              coords$y
            ))

            filters <- list(
              year_min = isolate(input$year_range[1]),
              year_max = isolate(input$year_range[2]),
              size_filter = isolate(input$size_filter),
              era_filter = isolate(input$era_filter),
              name_search = isolate(input$name_search)
            )

            tile_blob <- generate_meteorite_tile(
              con,
              coords$z,
              coords$x,
              coords$y,
              filters
            )
            message(sprintf("Tile size: %d bytes", length(tile_blob)))

            list(
              status = 200L,
              headers = list(
                "Content-Type" = "application/vnd.mapbox-vector-tile",
                "Access-Control-Allow-Origin" = "*",
                "Cache-Control" = "no-cache"
              ),
              body = tile_blob
            )
          } else if (path == "/tiles.json") {
            tilejson <- sprintf(
              '{
              "tilejson": "3.0.0",
              "tiles": ["http://127.0.0.1:%d/tiles/{z}/{x}/{y}.pbf"],
              "minzoom": 0,
              "maxzoom": 14
            }',
              TILE_SERVER_PORT
            )
            list(
              status = 200L,
              headers = list(
                "Content-Type" = "application/json",
                "Access-Control-Allow-Origin" = "*"
              ),
              body = tilejson
            )
          } else {
            list(status = 404L, body = "Not Found")
          }
        }
      )

      tile_server <<- startDaemonizedServer(
        "127.0.0.1",
        TILE_SERVER_PORT,
        tile_app
      )
      message("Tile server started on port ", TILE_SERVER_PORT)
    }
  })

  # Build WHERE clause
  build_where_clause <- function() {
    conditions <- c(
      sprintf(
        "year BETWEEN %d AND %d",
        input$year_range[1],
        input$year_range[2]
      ),
      "mass_tons > 0"
    )

    if (!is.null(input$name_search) && input$name_search != "") {
      conditions <- c(
        conditions,
        sprintf("name ILIKE '%%%s%%'", input$name_search)
      )
    }

    if (input$size_filter != "all") {
      conditions <- c(
        conditions,
        sprintf("size_category = '%s'", input$size_filter)
      )
    }

    if (input$era_filter != "all") {
      conditions <- c(conditions, sprintf("era = '%s'", input$era_filter))
    }

    paste(conditions, collapse = " AND ")
  }

  # Quick stats
  output$quick_stats <- renderText({
    input$apply_filters

    where_clause <- isolate(build_where_clause())

    stats <- dbGetQuery(
      con,
      sprintf(
        "
      SELECT 
        COUNT(*) as total,
        SUM(mass_tons) as total_mass,
        MEDIAN(mass_tons) as med_mass_tons
      FROM meteorites
      WHERE %s
    ",
        where_clause
      )
    )

    if (stats$total == 0) {
      return("No meteorites match filters")
    }

    heaviest <- dbGetQuery(
      con,
      sprintf(
        "
      SELECT name, mass_tons
      FROM meteorites
      WHERE %s
      ORDER BY mass_tons DESC, catalog_id ASC
      LIMIT 1
    ",
        where_clause
      )
    )

    paste0(
      "Meteorites: ",
      format(stats$total, big.mark = ","),
      "\nTotal Mass: ",
      format_mass(stats$total_mass),
      "\nMedian Mass: ",
      format_mass(stats$med_mass_tons),
      "\nHeaviest: ",
      heaviest$name
    )
  })

  # Mass distribution
  output$mass_histogram <- renderPlotly({
    input$apply_filters

    where_clause <- isolate(build_where_clause())

    data <- dbGetQuery(
      con,
      sprintf(
        "
      SELECT mass_tons
      FROM meteorites
      WHERE %s
    ",
        where_clause
      )
    )

    if (nrow(data) == 0) {
      return(
        plot_ly() %>%
          add_annotations(
            text = "No data",
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
      )
    }

    log_mass <- log10(data$mass_tons)
    dens <- density(log_mass, na.rm = TRUE)

    plot_ly() %>%
      add_lines(
        x = 10^dens$x,
        y = dens$y,
        fill = 'tozeroy',
        fillcolor = 'rgba(102, 126, 234, 0.3)',
        line = list(color = '#667eea', width = 2)
      ) %>%
      layout(
        xaxis = list(title = "Mass (tons)", type = "log", showgrid = FALSE),
        yaxis = list(title = "Density", showgrid = FALSE),
        margin = list(l = 40, r = 10, t = 10, b = 40),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Initialize map
  output$map <- renderMaplibre({
    maplibre(
      center = c(11, -11),
      zoom = 1,
      style = "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json",
      projection = "mercator"
    ) |>
      add_vector_source(
        id = "meteorites",
        tiles = paste0(
          "http://127.0.0.1:",
          TILE_SERVER_PORT,
          "/tiles/{z}/{x}/{y}.pbf"
        ),
        minzoom = 0,
        maxzoom = 14
      ) |>
      add_circle_layer(
        id = "meteorites-circles",
        source = "meteorites",
        source_layer = "meteorites",
        circle_radius = list(
          "base" = 1.5,
          "stops" = list(
            list(0, 3),
            list(5, 5),
            list(10, 7)
          )
        ),
        circle_color = list(
          "property" = "size_category",
          "type" = "categorical",
          "stops" = list(
            list("Tiny (< 1kg)", "#4A5568"),
            list("Small (1-5kg)", "#F7DC6F"),
            list("Medium (5-20kg)", "#F39C12"),
            list("Large (20kg-5t)", "#E74C3C"),
            list("Massive (> 5t)", "#C0392B")
          )
        ),
        circle_opacity = 0.8,
        circle_stroke_width = 1,
        circle_stroke_color = "#ffffff",
        popup = "info",
        hover_options = list(
          circle_radius = 10,
          circle_opacity = 1
        )
      ) |>
      add_navigation_control(position = "top-left")
  })

  # Reload tiles when filters applied
  observeEvent(input$apply_filters, {
    maplibre_proxy("map") |>
      set_layout_property("meteorites-circles", "visibility", "none")

    Sys.sleep(0.1)

    maplibre_proxy("map") |>
      set_layout_property("meteorites-circles", "visibility", "visible")
  })

  # Reset filters
  observeEvent(input$reset_filters, {
    updateSliderInput(
      session,
      "year_range",
      value = c(metadata$min_year, metadata$max_year)
    )
    updateTextInput(session, "name_search", value = "")
    updateSelectInput(session, "size_filter", selected = "Large (20kg-5t)")
    updateSelectInput(session, "era_filter", selected = "all")
  })

  # Timeline modal
  observeEvent(input$show_timeline, {
    where_clause <- build_where_clause()

    data <- dbGetQuery(
      con,
      sprintf(
        "
      SELECT name, mass_tons, year, lat, lon, era, reclass
      FROM meteorites
      WHERE %s
      LIMIT 10000
    ",
        where_clause
      )
    )

    if (nrow(data) > 0) {
      data <- data %>%
        mutate(hemisphere = ifelse(lat >= 0, "Northern", "Southern"))

      p <- plot_ly(
        data,
        x = ~year,
        y = ~mass_tons,
        color = ~hemisphere,
        colors = c("Northern" = "#2E86AB", "Southern" = "#A23B72"),
        text = ~ paste(
          "Name:",
          name,
          "<br>Mass:",
          round(mass_tons, 2),
          "tons",
          "<br>Year:",
          year,
          "<br>Type:",
          reclass
        ),
        hovertemplate = "%{text}<extra></extra>",
        type = "scatter",
        mode = "markers",
        marker = list(size = 8, opacity = 0.7)
      ) %>%
        layout(
          title = "Meteorite Timeline",
          xaxis = list(title = "Year"),
          yaxis = list(title = "Mass (tons)", type = "log")
        )

      showModal(modalDialog(
        title = "📈 Discovery Timeline",
        plotlyOutput("timeline_plot_modal", height = "500px"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))

      output$timeline_plot_modal <- renderPlotly({
        p
      })
    }
  })

  # Data table modal
  observeEvent(input$show_table, {
    where_clause <- build_where_clause()

    data <- dbGetQuery(
      con,
      sprintf(
        "
      SELECT name, mass_tons, nametype, reclass, year, fall, era, lat, lon, lpi_entry, mass
      FROM meteorites
      WHERE %s
      LIMIT 5000
    ",
        where_clause
      )
    )

    if (nrow(data) > 0) {
      data <- data %>%
        mutate(
          hemisphere = ifelse(lat >= 0, "Northern", "Southern"),
          mass_display = format_mass(mass_tons),
          name = paste0(
            '<a href="',
            lpi_entry,
            '" target="_blank">',
            name,
            '</a>'
          )
        ) %>%
        select(
          name,
          mass_display,
          reclass,
          year,
          nametype,
          fall,
          era,
          hemisphere,
          mass
        )

      showModal(modalDialog(
        title = "🔬 Meteorite Data",
        DTOutput("data_table_modal"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))

      output$data_table_modal <- renderDT({
        datatable(
          data,
          escape = FALSE,
          colnames = c(
            "Name",
            "Mass",
            "Type",
            "Year",
            "Status",
            "Discovery",
            "Era",
            "Hemisphere",
            "Mass (g)"
          ),
          options = list(
            pageLength = 15,
            order = list(list(8, 'desc')),
            scrollX = TRUE
          ),
          rownames = FALSE
        )
      })
    }
  })

  # Cleanup
  onStop(function() {
    if (!is.null(tile_server)) {
      message("Stopping tile server...")
      stopDaemonizedServer(tile_server)
    }
    message("Disconnecting from DuckDB...")
    dbDisconnect(con, shutdown = TRUE)
  })
}

# ============================================================================
# Run App
# ============================================================================

shinyApp(ui = ui, server = server)
