library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(duckdb)
library(DBI)

# Connect to spatial DuckDB database
con <- dbConnect(duckdb(), "data/meteorites_spatial.duckdb", read_only = TRUE)
dbExecute(con, "LOAD spatial;")

# Get initial bounds and metadata
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

# Get size categories for filter
size_categories <- c(
  "All" = "all",
  "Tiny (< 1kg)" = "Tiny (< 1kg)",
  "Small (1-5kg)" = "Small (1-5kg)",
  "Medium (5-20kg)" = "Medium (5-20kg)",
  "Large (20kg-5t)" = "Large (20kg-5t)",
  "Massive (> 5t)" = "Massive (> 5t)"
)

ui <- fluidPage(
  # Custom CSS for styling
  tags$head(
    tags$style(HTML(
      "
      .content-wrapper {
          background: linear-gradient(135deg, #6b7280 0%, #4b5563 50%, #374151 100%);
          min-height: 100vh;
          padding: 20px 0;
      }
      .main-title {
        color: #f3f4f6;
        text-align: center;
        font-size: 2.5em;
        margin-bottom: 30px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
      }
      .panel {
        background: #f3f4f6;
        border-radius: 10px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .sidebar-panel {
        background: rgba(255,255,255,0.95);
        border-radius: 10px;
        border: none;
        padding: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .stats-box {
        background: #f8f9fa;
        border: none;
        padding: 15px;
        margin: 15px 0;
        border-radius: 5px;
      }
    "
    ))
  ),

  div(
    class = "content-wrapper",
    div(
      class = "container-fluid",
      div(class = "main-title", "🌌 Meteorite Explorer: Rocks from Space!"),

      fluidRow(
        column(
          3,
          div(
            class = "sidebar-panel",
            h4("🔍 Search Options"),

            sliderInput(
              "year_range",
              "Discovery Year Range:",
              sep = "",
              min = metadata$min_year,
              max = metadata$max_year,
              value = c(metadata$min_year, metadata$max_year),
              step = 10
            ),

            textInput(
              "name_search",
              "🔍 Search by Name:",
              value = "",
              placeholder = "Enter meteorite name..."
            ),

            selectInput(
              "size_filter",
              "Size Category:",
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

            hr(),

            h5("📊 Quick Stats"),
            div(class = "stats-box", verbatimTextOutput("quick_stats")),

            hr(),

            h5("📊 Mass Distribution"),
            plotlyOutput("mass_histogram", height = "200px"),

            hr(),

            p(
              "Click on the bright meteorite markers to see detailed information! 🔥"
            ),
            div(
              class = "text-muted small mt-2",
              "More Info and Name links connect to official ",
              a(
                "Meteoritical Bulletin Database",
                href = "https://www.lpi.usra.edu/meteor/metbull.php",
                target = "_blank"
              ),
              " entries."
            ),

            hr(),

            h5("📖 About"),
            div(
              class = "stats-box",
              p(
                style = "margin-bottom: 10px; font-size: 0.9em;",
                "Data from ",
                a(
                  "NASA's Meteorite Database",
                  href = "https://data.nasa.gov/dataset/meteorite-landings",
                  target = "_blank",
                  style = "color: #667eea;"
                )
              ),
              p(
                style = "margin: 0; font-size: 0.9em;",
                "Total meteorites in database: ",
                format(metadata$total_count, big.mark = ",")
              )
            )
          )
        ),

        column(
          9,
          fluidRow(
            column(
              6,
              div(
                class = "panel",
                div(
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  h4("🗺️ Global Meteorite Distribution"),
                  actionButton(
                    "expand_map",
                    "⛶",
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Expand map"
                  )
                ),
                leafletOutput("meteorite_map", height = "400px")
              )
            ),
            column(
              6,
              div(
                class = "panel",
                div(
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  h4("📈 Discovery Timeline & Mass Distribution"),
                  actionButton(
                    "expand_timeline",
                    "⛶",
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Expand timeline"
                  )
                ),
                plotlyOutput("timeline_plot", height = "400px")
              )
            )
          ),

          fluidRow(
            column(
              12,
              div(
                class = "panel",
                h4("🔬 Meteorite Landings"),
                DTOutput("meteorite_table")
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Helper function to format mass with appropriate units
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

  # Track current map bounds for spatial filtering
  map_bounds <- reactiveVal(NULL)

  # Update bounds when map moves
  observeEvent(input$meteorite_map_bounds, {
    map_bounds(input$meteorite_map_bounds)
  })

  # Build SQL WHERE clause from filters
  build_where_clause <- function(use_spatial = FALSE) {
    conditions <- c("1=1") # Always true starting condition

    # Year filter
    conditions <- c(
      conditions,
      sprintf(
        "year BETWEEN %d AND %d",
        input$year_range[1],
        input$year_range[2]
      )
    )

    # Name search
    if (!is.null(input$name_search) && input$name_search != "") {
      conditions <- c(
        conditions,
        sprintf("name ILIKE '%%%s%%'", input$name_search)
      )
    }

    # Size filter
    if (input$size_filter != "all") {
      conditions <- c(
        conditions,
        sprintf("size_category = '%s'", input$size_filter)
      )
    }

    # Era filter
    if (input$era_filter != "all") {
      conditions <- c(conditions, sprintf("era = '%s'", input$era_filter))
    }

    # Spatial bounds filter (for map)
    if (use_spatial && !is.null(map_bounds())) {
      bounds <- map_bounds()
      conditions <- c(
        conditions,
        sprintf(
          "ST_Within(geom, ST_MakeEnvelope(%f, %f, %f, %f))",
          bounds$west,
          bounds$south,
          bounds$east,
          bounds$north
        )
      )
    }

    paste(conditions, collapse = " AND ")
  }

  # Query data based on filters
  filtered_data <- reactive({
    where_clause <- build_where_clause(use_spatial = FALSE)

    query <- sprintf(
      "
      SELECT 
        index, name, catalog_id, nametype, reclass, mass, fall,
        year, lat, lon, state, county, lpi_entry,
        mass_kg, mass_tons, size_category, era
      FROM meteorites
      WHERE %s
    ",
      where_clause
    )

    dbGetQuery(con, query)
  })

  # Query map data with spatial filter
  map_data <- reactive({
    where_clause <- build_where_clause(use_spatial = TRUE)

    query <- sprintf(
      "
      SELECT 
        name, reclass, mass_tons, year, lat, lon, lpi_entry, size_category
      FROM meteorites
      WHERE %s
      LIMIT 5000
    ",
      where_clause
    )

    dbGetQuery(con, query)
  })

  # Quick stats
  output$quick_stats <- renderText({
    where_clause <- build_where_clause(use_spatial = FALSE)

    stats <- dbGetQuery(
      con,
      sprintf(
        "
      SELECT 
        COUNT(*) as total,
        SUM(mass_tons) as total_mass,
        AVG(mass_tons) as avg_mass,
        MEDIAN(mass_tons) as med_mass,
        MEDIAN(mass_tons) as med_mass_tons,
      FROM meteorites
      WHERE %s
      AND mass_tons > 0
    ",
        where_clause
      )
    )

    if (stats$total == 0) {
      return("No meteorites match current filters")
    }

    heaviest <- dbGetQuery(
      con,
      sprintf(
        "
      SELECT name 
      FROM meteorites
      WHERE %s
      ORDER BY mass_tons DESC
      LIMIT 1
    ",
        where_clause
      )
    )

    paste0(
      "Meteorites: ",
      format(stats$total, big.mark = ","),
      "\n",
      "Total Mass: ",
      round(stats$total_mass, 1),
      " tons\n",
      "Median Mass: ",
      format_mass(stats$med_mass_tons),
      "\nHeaviest: ",
      heaviest$name
    )
  })

  # Mass distribution density
  output$mass_histogram <- renderPlotly({
    where_clause <- build_where_clause(use_spatial = FALSE)

    data <- dbGetQuery(
      con,
      sprintf(
        "
    SELECT mass_tons
    FROM meteorites
    WHERE %s
    AND mass_tons > 0
  ",
        where_clause
      )
    )

    if (nrow(data) == 0 || is.null(data$mass_tons)) {
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

    # Calculate density on log scale
    log_mass <- log10(data$mass_tons)
    dens <- density(log_mass, na.rm = TRUE)

    plot_ly() %>%
      add_lines(
        x = 10^dens$x, # Convert back to original scale
        y = dens$y,
        fill = 'tozeroy',
        fillcolor = 'rgba(102, 126, 234, 0.3)',
        line = list(color = '#667eea', width = 2)
      ) %>%
      layout(
        xaxis = list(
          title = "Mass (tons)",
          type = "log",
          showgrid = FALSE
        ),
        yaxis = list(
          title = "Density",
          showgrid = FALSE
        ),
        margin = list(l = 40, r = 10, t = 10, b = 40),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Create shared map reactive
  shared_map <- reactive({
    data <- map_data()

    if (nrow(data) == 0) {
      return(
        leaflet() %>%
          addTiles() %>%
          setView(lng = 0, lat = 20, zoom = 2) %>%
          addPopups(lng = 0, lat = 0, "No data matches current filters")
      )
    }

    # Calculate map center
    center_lat <- mean(data$lat, na.rm = TRUE)
    center_lng <- mean(data$lon, na.rm = TRUE)

    # Calculate zoom
    lat_range <- max(data$lat, na.rm = TRUE) - min(data$lat, na.rm = TRUE)
    lng_range <- max(data$lon, na.rm = TRUE) - min(data$lon, na.rm = TRUE)
    zoom_level <- if (max(lat_range, lng_range) > 100) {
      2
    } else if (max(lat_range, lng_range) > 50) {
      3
    } else {
      4
    }

    # Create mass category for coloring
    data <- data %>%
      mutate(
        mass_category = cut(
          mass_tons,
          breaks = c(0, 10, 50, 200, Inf),
          labels = c("< 10 tons", "10-50 tons", "50-200 tons", "> 200 tons"),
          include.lowest = TRUE
        )
      )

    pal <- colorFactor(
      palette = c("#4A5568", "#F7DC6F", "#F39C12", "#E74C3C"),
      domain = levels(data$mass_category)
    )
    # Create popups
    data$mass_display <- format_mass(data$mass_tons)

    popups <- paste0(
      "<div style='font-size: 14px; line-height: 1.4;'>",
      "<h4 style='margin: 0 0 10px 0; color: #2c3e50;'>🌠 ",
      data$name,
      "</h4>",
      "<p style='margin: 5px 0;'><strong>Type:</strong> ",
      data$reclass,
      "</p>",
      "<p style='margin: 5px 0;'><strong>Mass:</strong> ",
      data$mass_display, # Use formatted mass
      "</p>",
      "<p style='margin: 5px 0;'><strong>Year:</strong> ",
      data$year,
      "</p>",
      "<p style='margin: 5px 0;'><strong>Location:</strong> ",
      round(data$lat, 3),
      "°, ",
      round(data$lon, 3),
      "°</p>",
      "<p style='margin: 10px 0 0 0;'><a href='",
      data$lpi_entry,
      "' target='_blank' style='color: #667eea;'>🔗 More Info</a></p>",
      "</div>"
    )

    # Calculate radius
    radius_vals <- 5 +
      (log10(pmax(data$mass_tons, 0.1)) /
        log10(max(data$mass_tons, na.rm = TRUE))) *
        10
    radius_vals[!is.finite(radius_vals)] <- 8
    radius_vals <- pmax(pmin(radius_vals, 15), 5)

    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon,
        ~lat,
        radius = radius_vals,
        color = "white",
        weight = 2,
        fillColor = ~ pal(mass_category),
        fillOpacity = 0.8,
        popup = popups,
        label = ~ paste0(name, " (", round(mass_tons, 1), " tons)")
      ) %>%
      addLegend(
        pal = pal,
        values = ~mass_category,
        title = "Mass Categories",
        position = "bottomright",
        opacity = 0.8
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })

  # Create shared timeline reactive
  shared_timeline <- reactive({
    data <- filtered_data()

    if (nrow(data) == 0) {
      return(
        plot_ly() %>%
          add_annotations(
            text = "No data matches filters",
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
      )
    }

    # Create hemisphere categories
    data <- data %>%
      mutate(
        hemisphere = ifelse(
          lat >= 0,
          "Northern Hemisphere",
          "Southern Hemisphere"
        )
      )

    hemisphere_colors <- c(
      "Northern Hemisphere" = "#2E86AB",
      "Southern Hemisphere" = "#A23B72"
    )

    year_min <- min(data$year, na.rm = TRUE)
    year_max <- max(data$year, na.rm = TRUE)
    year_padding <- max(1, (year_max - year_min) * 0.02)

    plot_ly(
      data,
      x = ~year,
      y = ~mass_tons,
      color = ~hemisphere,
      colors = hemisphere_colors,
      text = ~ paste(
        "Name:",
        name,
        "<br>Mass:",
        format(round(mass_tons, 2), big.mark = ","),
        "tons",
        "<br>Year:",
        year,
        "<br>Era:",
        era,
        "<br>Hemisphere:",
        hemisphere,
        "<br>Location:",
        round(lat, 2),
        "°, ",
        round(lon, 2),
        "°"
      ),
      hovertemplate = "%{text}<extra></extra>",
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 14,
        line = list(width = 1, color = "white"),
        opacity = 0.7
      )
    ) %>%
      layout(
        title = list(
          text = "Meteorite Discoveries: Timeline by Hemisphere",
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Discovery Year",
          showgrid = TRUE,
          range = c(year_min - year_padding, year_max + year_padding)
        ),
        yaxis = list(
          title = "Mass (tons)",
          type = "linear",
          showgrid = TRUE
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.3,
          y = 1.02,
          font = list(size = 12),
          itemclick = "toggle",
          itemdoubleclick = "toggleothers"
        ),
        hovermode = "closest",
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(
        displayModeBar = FALSE,
        showTips = FALSE
      )
  })

  # Outputs
  output$meteorite_map <- renderLeaflet({
    shared_map()
  })

  output$meteorite_map_full <- renderLeaflet({
    shared_map()
  })

  output$timeline_plot <- renderPlotly({
    shared_timeline()
  })

  output$timeline_plot_full <- renderPlotly({
    shared_timeline()
  })

  # Modal event handlers
  observeEvent(input$expand_map, {
    showModal(modalDialog(
      title = "🗺️ Global Meteorite Distribution - Full View",
      leafletOutput("meteorite_map_full", height = "70vh"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$expand_timeline, {
    showModal(modalDialog(
      title = "📈 Discovery Timeline - Full View",
      plotlyOutput("timeline_plot_full", height = "70vh"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # Data table
  output$meteorite_table <- renderDT({
    data <- filtered_data() %>%
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
        scrollX = TRUE,
        order = list(list(8, 'desc')),
        dom = 'ltip'
      ),
      rownames = FALSE
    )
  })
}

# Close connection on app stop
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
})

shinyApp(ui = ui, server = server)
