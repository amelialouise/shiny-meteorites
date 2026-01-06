library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(duckdb)
library(DBI)

source("R/helpers.R", local = TRUE)

ui <- fluidPage(
  # Custom CSS for styling
  tags$head(
    tags$style(HTML(
      "
      .content-wrapper {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        min-height: 100vh;
        padding: 20px 0;
      }
      .main-title {
        color: white;
        text-align: center;
        font-size: 2.5em;
        margin-bottom: 30px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
      }
      .subtitle {
        color: rgba(255,255,255,0.9);
        text-align: center;
        font-size: 1.2em;
        margin-bottom: 20px;
      }
      .panel {
        background: white;
        border-radius: 10px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .sidebar-panel {
        background: rgba(255,255,255,0.95);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .stats-box {
        background: #f8f9fa;
        border-left: 4px solid #667eea;
        padding: 15px;
        margin: 15px 0;
        border-radius: 5px;
      }
      .info-badge {
        background: #e74c3c;
        color: white;
        padding: 5px 10px;
        border-radius: 15px;
        font-size: 0.9em;
        margin: 10px 0;
        display: inline-block;
      }
    "
    ))
  ),

  div(
    class = "content-wrapper",
    div(
      class = "container-fluid",
      div(class = "main-title", "🌌 Meteorite Explorer: Giants from Space"),
      div(
        class = "subtitle",
        paste0(
          "🔥 Curated selection of ",
          format(total_in_db, big.mark = ","),
          " fascinating meteorites from ",
          format(total_in_db, big.mark = ","),
          " total discoveries"
        )
      ),

      fluidRow(
        column(
          3,
          div(
            class = "sidebar-panel",
            h4("🔍 Explore Controls"),

            div(
              class = "info-badge",
              "Smart filtered for performance & interest!"
            ),

            sliderInput(
              "year_range",
              "Discovery Year Range:",
              min = min(meteorites_sm$year, na.rm = TRUE),
              max = max(meteorites_sm$year, na.rm = TRUE),
              value = c(
                min(meteorites_sm$year, na.rm = TRUE),
                max(meteorites_sm$year, na.rm = TRUE)
              ),
              step = 10
            ),

            selectInput(
              "era_filter",
              "Discovery Era:",
              choices = c("All Eras" = "all", unique(meteorites_sm$era)),
              selected = "all"
            ),

            checkboxGroupInput(
              "classification",
              "Meteorite Types:",
              choices = sort(unique(meteorites_sm$reclass))[1:10], # Limit to top 10 types
              selected = sort(unique(meteorites_sm$reclass))[1:10]
            ),

            selectInput(
              "size_filter",
              "Size Category:",
              choices = c(
                "All Sizes" = "all",
                unique(meteorites_sm$size_category)
              ),
              selected = "all"
            ),

            hr(),

            h5("📊 Quick Stats"),
            div(class = "stats-box", verbatimTextOutput("quick_stats")),

            hr(),

            p(
              "💡 This curated dataset focuses on the most interesting meteorites: massive specimens, historical finds, and notable recent discoveries! 🔥"
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
                h4("🗺️ Global Meteorite Distribution"),
                leafletOutput("meteorite_map", height = "450px")
              )
            ),
            column(
              6,
              div(
                class = "panel",
                h4("📈 Discovery Timeline & Mass"),
                plotlyOutput("timeline_plot", height = "450px")
              )
            )
          ),

          fluidRow(
            column(
              12,
              div(
                class = "panel",
                h4("🔬 Notable Meteorites Database"),
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
  # In-memory duckdb connection!
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = TRUE)

  initialize_database(con, "data/meteorites.duckdb", table = "meteorites")

  meteorites <-
    dplyr::tbl(con, "meteorites") %>%
    mutate(
      year = as.integer(year),
      mass_kg = mass / 1000,
      mass_tons = mass / 1000000,
      size_category = case_when(
        mass_tons < 0.001 ~ "Tiny (< 1kg)",
        mass_tons < 0.005 ~ "Small (1-5kg)",
        mass_tons < 0.02 ~ "Medium (5-20kg)",
        mass_tons < 5 ~ "Large (20kg-5t)",
        TRUE ~ "Massive (> 5t)"
      ),
      era = case_when(
        year < 1800 ~ "Ancient (pre-1800)",
        year < 1900 ~ "Historical (1800-1899)",
        year < 1950 ~ "Early Modern (1900-1949)",
        year < 2000 ~ "Late Modern (1950-1999)",
        TRUE ~ "Recent (2000+)"
      )
    )

  # Reactive filtered data with optimizations
  filtered_data <- reactive({
    data <- meteorites %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2])

    if (input$era_filter != "all") {
      data <- data %>% filter(era == input$era_filter)
    }

    if (length(input$classification) > 0) {
      data <- data %>% filter(reclass %in% input$classification)
    }

    if (input$size_filter != "all") {
      data <- data %>% filter(size_category == input$size_filter)
    }

    return(data)
  })

  # Quick stats
  output$quick_stats <- renderText({
    data <- filtered_data()
    total_meteorites <- nrow(data)
    total_mass <- sum(data$mass_kg, na.rm = TRUE)
    avg_mass <- round(mean(data$mass_kg, na.rm = TRUE), 3)

    heaviest_name <- if (nrow(data) > 0) {
      data$name[which.max(data$mass_kg)]
    } else {
      "None"
    }

    paste0(
      "Displayed: ",
      total_meteorites,
      "\n",
      "Total Mass: ",
      format(round(total_mass, 1), big.mark = ","),
      " tons\n",
      "Avg Mass: ",
      avg_mass,
      " tons\n",
      "Heaviest: ",
      heaviest_name,
      "\n",
      "Years: ",
      min(data$year, na.rm = TRUE),
      "-",
      max(data$year, na.rm = TRUE)
    )
  })

  # Optimized interactive map
  output$meteorite_map <- renderLeaflet({
    data <- filtered_data()

    if (nrow(data) == 0) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = 0, lat = 20, zoom = 2) %>%
        addPopups(
          lng = 0,
          lat = 0,
          popup = "No meteorites match current filters!"
        )
      return()
    }

    # Limit map display to reasonable number for performance
    if (nrow(data) > 1000) {
      # Sample the largest ones plus random selection
      large_ones <- data %>% arrange(desc(mass)) %>% slice_head(n = 500)
      random_ones <- data %>% slice_sample(n = 500)
      data <- bind_rows(large_ones, random_ones) %>%
        distinct(name, .keep_all = TRUE)
    }

    # Create color palette
    pal <- colorNumeric(palette = "plasma", domain = log10(data$mass + 1))

    # Enhanced popups
    popups <- paste0(
      "<div style='font-size: 14px; line-height: 1.4;'>",
      "<h4 style='margin: 0 0 10px 0; color: #2c3e50;'>🌠 ",
      data$name,
      "</h4>",
      "<p style='margin: 5px 0;'><strong>Type:</strong> ",
      data$reclass,
      "</p>",
      "<p style='margin: 5px 0;'><strong>Mass:</strong> ",
      format(data$mass_kg, big.mark = ","),
      " kg</p>",
      "<p style='margin: 5px 0;'><strong>Era:</strong> ",
      data$era,
      "</p>",
      "<p style='margin: 5px 0;'><strong>Size:</strong> ",
      data$size_category,
      "</p>",
      "<p style='margin: 5px 0;'><strong>Location:</strong> ",
      round(data$lat, 3),
      "°, ",
      round(data$long, 3),
      "°</p>",
      "</div>"
    )

    # Dynamic radius with better scaling for the larger dataset
    radius_scaling <- pmax(
      8,
      pmin(
        25,
        8 +
          (log10(data$mass + 1) /
            max(log10(data$mass + 1), na.rm = TRUE)) *
            17
      )
    )

    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~long,
        ~lat,
        radius = radius_scaling,
        color = "white",
        weight = 3,
        fillColor = ~ pal(log10(mass + 1)),
        fillOpacity = 0.8,
        popup = popups,
        popupOptions = popupOptions(maxWidth = 300, closeButton = TRUE),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        label = ~ paste0(
          "🌠 ",
          name,
          " (",
          format(mass_kg, big.mark = ","),
          " kg)"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~ log10(mass + 1),
        title = "Mass (log scale)",
        position = "bottomright",
        labFormat = labelFormat(transform = function(x) {
          paste0(format(round(10^x), big.mark = ","), "g")
        })
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })

  # Enhanced timeline visualization
  output$timeline_plot <- renderPlotly({
    data <- filtered_data()

    if (nrow(data) == 0) {
      plot_ly() %>%
        add_text(
          x = 0.5,
          y = 0.5,
          text = "No data matches current filters",
          textfont = list(size = 16)
        ) %>%
        layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
      return()
    }

    p <- plot_ly(
      data,
      x = ~year,
      y = ~mass_kg,
      size = ~mass_kg,
      color = ~era,
      text = ~ paste(
        "Name:",
        name,
        "<br>Mass:",
        format(mass_kg, big.mark = ","),
        "kg",
        "<br>Year:",
        year,
        "<br>Era:",
        era,
        "<br>Type:",
        reclass
      ),
      hovertemplate = "%{text}<extra></extra>",
      type = "scatter",
      mode = "markers"
    ) %>%
      layout(
        title = list(
          text = "Meteorite Discoveries Over Time",
          font = list(size = 14)
        ),
        xaxis = list(title = "Discovery Year"),
        yaxis = list(title = "Mass (kg)", type = "log"),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.02, y = 1)
      ) %>%
      config(displayModeBar = FALSE)

    p
  })

  # Enhanced data table
  output$meteorite_table <- renderDT({
    data <- filtered_data() %>%
      select(name, reclass, mass_kg, year, era, size_category, lat, long) %>%
      mutate(
        mass_kg = round(mass_kg, 2),
        lat = round(lat, 3),
        long = round(long, 3)
      ) %>%
      arrange(desc(mass_kg)) # Show largest first

    datatable(
      data,
      colnames = c(
        "Name",
        "Type",
        "Mass (kg)",
        "Year",
        "Era",
        "Size",
        "Lat",
        "Long"
      ),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'ltip',
        order = list(list(2, 'desc')) # Sort by mass descending
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        "mass_kg",
        background = styleColorBar(data$mass_kg, "#667eea", angle = -90),
        backgroundSize = "100% 90%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle(
        "size_category",
        backgroundColor = styleEqual(
          c(
            "Tiny (< 1kg)",
            "Small (1-5kg)",
            "Medium (5-20kg)",
            "Large (20kg-5t)",
            "Massive (> 5t)"
          ),
          c("#f8f9fa", "#e3f2fd", "#bbdefb", "#90caf9", "#42a5f5")
        )
      )
  })

  # Disconnect from DuckDB when the app stops
  onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui = ui, server = server)
