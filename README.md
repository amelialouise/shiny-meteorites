# Meteorite Explorer

An interactive R Shiny application for exploring meteorite landings data from NASA. Visualize the global distribution of meteorite discoveries and patterns over time from as early as 861 ([Nogata](https://www.lpi.usra.edu/meteor/metbull.php?code=16988))!

![Shiny app screenshot](images/meteorites-explorer-screenshot.png)

## Features

-   **Interactive Global Map**: View meteorite landing locations worldwide with size-based markers and color-coded mass categories
-   **Easy Filtering**: Filter by discovery year range, meteorite name, and size 
-   **Quick Stats**: Get numbers on filtered data including total mass, average mass, and heaviest meteorite
-   **Responsive Design**: Clean, modern interface optimized for data exploration

## Data Source

This application uses meteorite landing data from:

\- **NASA Meteorite Landings Dataset (json)**: <https://data.nasa.gov/dataset/meteorite-landings>

\- Links to detailed information from the **Meteoritical Bulletin Database** maintained by the Lunar and Planetary Institute

## Tech Stack

-   **R Shiny**: Web application framework
-   **DuckDB**: High-performance analytical database for data processing
-   **Leaflet**: Interactive mapping
-   **Plotly**: Interactive visualizations
-   **DataTables**: Enhanced table display with search and sorting
-   **bslib**: Modern UI components and theming

## Dependencies

-   R (version 4.0 or higher)
-   Required R packages (see renv)

## Project Structure

```
Shiny implementation          # R Shiny 
│   └── app.R
├── R/                        # DuckDB via R client
├── download_meteorites.R     # Download data from NASA portal
├── build_indexed_db.R        # Build indexed DuckDB database
└── README.md
```

## 🔧 Getting Setup

### 1. Install R package dependencies
```r
install.packages(c("arrow", "here", "shiny", "leaflet", "dplyr", "stringr", "plotly", "duckdb", "DBI", "DT"))
```

Or use *renv*

```r
renv::restore()
```

### 2. Download Data
```bash
cd R
Rscript ./download_meteorites.R
Rscript ./build_indexed_db.R
```

### 3. Run Shiny App

```r
shiny::runApp("Shiny")
```

## Data Processing with DuckDB

The app uses DuckDB for efficient data processing:

**Load**: NASA meteorite data (JSON format)  
**Process**: Ingest, transform, and create local data files 
**Prep**: Create mass categories, filter missing data, and build index for spatial data

## Acknowledgments

- [Meteoritical Society](https://meteoritical.org/) - official meteorite database  
- [NASA](https://www.nasa.gov/) - meteorite landings dataset  
- [Max Gabrielsson](https://github.com/Maxxen) - DuckDB spatial extension  
- [Shiny Assistant](https://gallery.shinyapps.io/assistant/#) - Shiny app building and troubleshooting  

## References

- [Josiah Parry - {duckdb} or {dbplyr}](https://josiahparry.com/posts/2024-05-24-duckdb-and-r)  
- [Dario Radečić - R Shiny and DuckDB: How to Speed Up Your Shiny Apps When Working With Large Datasets](https://www.appsilon.com/post/r-shiny-duckd)
- [Sara Altman - Creating a Shiny app that interacts with a database ](https://posit.co/blog/shiny-with-databases/)

*note*: working on an update to this using a tile service, inspired by this post from Federico Tallis on [visualizing millions of buildings with duckdb](https://medium.com/@federico.tallis/visualizing-millions-of-buildings-with-duckdb-st-asmvt-a-streamlit-vs-shiny-comparison-5a4c924fe067)  
