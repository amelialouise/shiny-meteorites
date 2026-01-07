library(shiny)
library(dplyr)
library(duckdb)
library(DBI)
library(bslib)
library(DT)
library(plotly)
library(sf)
library(mapgl) # Load mapgl last to prioritize its functions


# ============================================================================
# Configuration
# ============================================================================

DB_PATH <- "data/meteorites.duckdb"
options(sass.cache = FALSE)

# ============================================================================
# Helper Functions
# ============================================================================

format_mass <- function(mass_tons) {
  case_when(
    mass_tons * 1e9 < 1e3 ~ paste0(round(mass_tons * 1e9, 1), " mg"),
    mass_tons * 1e6 < 1e3 ~ paste0(round(mass_tons * 1e6, 1), " g"),
    mass_tons * 1e3 < 1e3 ~ paste0(round(mass_tons * 1e3, 1), " kg"),
    .default = paste0(round(mass_tons, 1), " tons")
  )
}

get_size_color <- function(mass_tons) {
  case_when(
    mass_tons < 0.001 ~ "#4A5568", # Tiny - dark gray
    mass_tons < 0.005 ~ "#F7DC6F", # Small - light yellow
    mass_tons < 0.02 ~ "#F39C12", # Medium - orange
    mass_tons < 5 ~ "#E74C3C", # Large - red
    TRUE ~ "#C0392B" # Massive - dark red
  )
}

get_marker_size <- function(mass_tons) {
  pmax(3, pmin(15, log10(mass_tons * 1000) * 2 + 5))
}

# ============================================================================
# Connect to Database
# ============================================================================

message("Connecting to DuckDB: ", DB_PATH)
# Global connection variable
con <- NULL

# Function to ensure connection is valid
ensure_connection <- function() {
  if (is.null(con) || !dbIsValid(con)) {
    message("Reconnecting to database...")
    if (!is.null(con)) {
      try(dbDisconnect(con), silent = TRUE)
    }
    con <<- dbConnect(duckdb(), DB_PATH, read_only = TRUE)
  }
  return(con)
}

# Safe query function
safe_db_query <- function(query, default = data.frame()) {
  tryCatch(
    {
      conn <- ensure_connection()
      dbGetQuery(conn, query)
    },
    error = function(e) {
      message("Database error: ", e$message)
      showNotification(
        paste("Database error:", e$message),
        type = "error",
        duration = 5
      )
      return(default)
    }
  )
}

metadata <- safe_db_query(
  "SELECT MIN(year) as min_year, MAX(year) as max_year, COUNT(*) as total_count FROM meteorites"
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

  maplibreOutput("map", height = "100%"),

  conditionalPanel(
    condition = "$('html').hasClass('shiny-busy')",
    div(
      class = "text-center mt-2",
      HTML('<div class="spinner-border spinner-border-sm text-primary"></div>'),
      " Loading..."
    )
  ),

  absolutePanel(
    top = 20,
    right = 20,
    width = 360,
    class = "card shadow",
    style = "background: white; border-radius: 8px; padding: 20px; max-height: 90vh; overflow-y: auto;",

    h4("🌌 Meteorite Explorer", class = "mb-3"),

    div(
      class = "filter-section",
      h5("🔍 Click 'Apply Filters' to update the map"),

      # Era filter
      selectInput(
        "era_filter",
        "Time Period:",
        choices = c(
          "All Eras" = "all",
          "Unknown/Missing Years" = "unknown",
          "Ancient (861-1799)" = "ancient",
          "Historical (1800-1899)" = "historical",
          "Early Modern (1900-1949)" = "early_modern",
          "Late Modern (1950-1999)" = "late_modern",
          "Recent (2000+)" = "recent"
        ),
        selected = "late_modern" # Make sure this matches a valid choice
      ),

      textInput("name_search", "Name:", value = "", placeholder = "Search..."),

      selectInput(
        "discovery_filter",
        "Discovery:",
        choices = c("All" = "all", "Fell" = "Fell", "Found" = "Found"),
        selected = "all"
      ),

      selectInput(
        "size_filter",
        "Size:",
        choices = size_categories,
        selected = "Large (20kg-5t)"
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
  # Build WHERE clause function
  build_where_clause <- function() {
    # Handle era filtering with explicit year ranges
    year_condition <- switch(
      input$era_filter,
      "all" = "1=1",
      "unknown" = "year IS NULL",
      "ancient" = "year BETWEEN 861 AND 1799",
      "historical" = "year BETWEEN 1800 AND 1899",
      "early_modern" = "year BETWEEN 1900 AND 1949",
      "late_modern" = "year BETWEEN 1950 AND 1999",
      "recent" = "year >= 2000"
    )

    # If switch returned NULL (no match), use fallback
    if (is.null(year_condition)) {
      year_condition <- "1=1"
      cat("Using fallback condition\n")
    }

    # Debug: print the year condition
    # Always include mass filter since unknown years do have mass data
    conditions <- c(year_condition, "mass_tons > 0")

    if (!is.null(input$discovery_filter) && input$discovery_filter != "all") {
      conditions <- c(
        conditions,
        sprintf("fall = '%s'", input$discovery_filter)
      )
    }

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

    paste(conditions, collapse = " AND ")
  }

  # Get filtered data for map
  get_map_data <- reactive({
    input$apply_filters

    where_clause <- isolate(build_where_clause())

    tryCatch(
      {
        data <- safe_db_query(
          sprintf(
            "SELECT name, lat, lon, mass_tons, year, size_category, catalog_id, reclass, era, fall, lpi_entry
       FROM meteorites 
       WHERE %s 
       ORDER BY mass_tons DESC
       LIMIT 8000",
            where_clause
          )
        )

        # Add popup text column
        data$popup_text <- sprintf(
          "<b>%s</b><br>Mass: %s<br>Year: %d<br>Type: %s<br><a href='%s' target='_blank'>More info</a>",
          data$name,
          sapply(data$mass_tons, format_mass),
          data$year,
          data$reclass,
          data$lpi_entry
        )

        return(data)
      },
      error = function(e) {
        showNotification(
          paste("Error loading data:", e$message),
          type = "error"
        )
        data.frame()
      }
    )
  })

  # Initialize and update map
  output$map <- renderMaplibre({
    maplibre(
      center = c(11, -11),
      zoom = 1,
      style = "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json",
      projection = "mercator"
    ) %>%
      add_navigation_control(position = "top-left")
  })

  # Update map when data changes
  # Update map when data changes
  observeEvent(get_map_data(), {
    data <- get_map_data()

    if (nrow(data) == 0) {
      return()
    }

    # Simple approach: recreate the map with new data
    output$map <- renderMaplibre({
      if (nrow(data) == 0) {
        return(
          maplibre(
            center = c(11, -11),
            zoom = 1,
            style = "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json",
            projection = "mercator"
          ) %>%
            add_navigation_control(position = "top-left")
        )
      }

      # Convert to sf object
      data_sf <- data %>%
        filter(!is.na(lon), !is.na(lat)) %>%
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

      maplibre(
        center = c(mean(data$lon, na.rm = TRUE), mean(data$lat, na.rm = TRUE)),
        zoom = 2,
        style = "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json",
        projection = "mercator"
      ) %>%
        add_source(id = "meteorites", data = data_sf) %>%
        add_circle_layer(
          id = "meteorites-circles",
          source = "meteorites",
          circle_radius = list(
            "property" = "mass_tons",
            "type" = "exponential",
            "base" = 1.5,
            "stops" = list(
              list(0.001, 3),
              list(0.1, 5),
              list(1, 7),
              list(10, 10),
              list(100, 15)
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
          circle_opacity = 0.7,
          circle_stroke_width = 1,
          circle_stroke_color = "#ffffff",
          popup = "popup_text"
        ) %>%
        add_navigation_control(position = "top-left")
    })
  })

  # Quick stats
  output$quick_stats <- renderText({
    input$apply_filters

    where_clause <- isolate(build_where_clause())

    stats <- tryCatch(
      {
        safe_db_query(
          sprintf(
            "SELECT COUNT(*) as total, SUM(mass_tons) as total_mass, 
       percentile_cont(0.5) WITHIN GROUP (ORDER BY mass_tons) as med_mass_tons
       FROM meteorites WHERE %s",
            where_clause
          )
        )
      },
      error = function(e) {
        showNotification(paste("Stats error:", e$message), type = "error")
        data.frame(total = 0, total_mass = 0, med_mass_tons = 0)
      }
    )

    if (stats$total == 0) {
      return("No meteorites match filters")
    }

    heaviest <- tryCatch(
      {
        safe_db_query(
          sprintf(
            "SELECT name, mass_tons FROM meteorites WHERE %s 
       ORDER BY mass_tons DESC, catalog_id ASC LIMIT 1",
            where_clause
          )
        )
      },
      error = function(e) {
        data.frame(name = "Unknown", mass_tons = 0)
      }
    )

    # Special note for unknown years
    note_text <- if (input$era_filter == "unknown") {
      "\n(Unknown discovery years)"
    } else {
      ""
    }

    paste0(
      "Meteorites: ",
      format(stats$total, big.mark = ","),
      "\nTotal Mass: ",
      format_mass(stats$total_mass),
      "\nMedian Mass: ",
      format_mass(stats$med_mass_tons),
      "\nHeaviest: ",
      heaviest$name,
      note_text
    )
  })

  # Mass distribution (same as before)
  output$mass_histogram <- renderPlotly({
    input$apply_filters

    where_clause <- isolate(build_where_clause())

    data <- tryCatch(
      {
        safe_db_query(
          sprintf(
            "SELECT mass_tons, size_category FROM meteorites WHERE %s",
            where_clause
          )
        )
      },
      error = function(e) {
        data.frame(mass_tons = numeric(0), size_category = character(0))
      }
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

    plot_ly(
      data,
      x = ~mass_tons,
      type = "histogram",
      marker = list(color = "#667eea", line = list(color = "white", width = 1)),
      nbinsx = 50
    ) %>%
      layout(
        xaxis = list(title = "Mass (tons)", type = "log10", showgrid = FALSE),
        yaxis = list(title = "Count", showgrid = FALSE),
        margin = list(l = 40, r = 10, t = 10, b = 40),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "discovery_filter", selected = "all")
    updateTextInput(session, "name_search", value = "")
    updateSelectInput(session, "size_filter", selected = "all")
    updateSelectInput(session, "era_filter", selected = "all") # Updated value
  })

  # Timeline modal
  observeEvent(input$show_timeline, {
    where_clause <- build_where_clause()

    data <- tryCatch(
      {
        safe_db_query(
          sprintf(
            "SELECT name, mass_tons, year, lat, lon, era, reclass
         FROM meteorites WHERE %s LIMIT 10000",
            where_clause
          )
        )
      },
      error = function(e) {
        showNotification("Error loading timeline data", type = "error")
        return()
      }
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

    data <- tryCatch(
      {
        safe_db_query(
          sprintf(
            "SELECT name, mass_tons, nametype, reclass, year, fall, era, lat, lon, lpi_entry, mass
         FROM meteorites WHERE %s LIMIT 5000",
            where_clause
          )
        )
      },
      error = function(e) {
        showNotification("Error loading table data", type = "error")
        return()
      }
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
            pageLength = 10,
            order = list(list(8, 'desc')),
            scrollX = TRUE
          ),
          rownames = FALSE
        )
      })
    }
  })

  # At the end of your server function, add:
  onStop(function() {
    message("App stopping - cleaning up database connection...")
    if (!is.null(con) && dbIsValid(con)) {
      try(
        {
          dbDisconnect(con, shutdown = TRUE)
          message("Database connection closed successfully")
        },
        silent = TRUE
      )
    } else {
      message("No valid connection to close")
    }
  })
}

# ============================================================================
# Run App
# ============================================================================

shinyApp(ui = ui, server = server)
