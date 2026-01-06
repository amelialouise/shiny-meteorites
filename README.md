# 🌌 Meteorite Explorer

An interactive R Shiny application for exploring meteorite landings data from NASA. Visualize the global distribution of meteorite discoveries and patterns over time from as early as 861 ([Nogata](https://www.lpi.usra.edu/meteor/metbull.php?code=16988))!

![Shiny app screenshot](images/meteorites-explorer-screenshot.png)

## 🚀 Features

-   **Interactive Global Map**: View meteorite landing locations worldwide with size-based markers and color-coded mass categories
-   **Discovery Timeline**: Analyze meteorite discovery patterns over time, with data split by hemisphere
-   **Easy Filtering**: Filter by discovery year range, meteorite name, and size category
-   **Detailed Database View**: Browse rich meteorite data with clickable links to official database entries
-   **Quick Stats**: Get numbers on filtered data including total mass, average mass, and heaviest meteorite
-   **Responsive Design**: Clean, modern interface optimized for data exploration

## 📊 Data Source

This application uses meteorite landing data from:

\- **NASA Meteorite Landings Dataset (json)**: <https://data.nasa.gov/dataset/meteorite-landings>

\- Links to detailed information from the **Meteoritical Bulletin Database** maintained by the Lunar and Planetary Institute

## 🛠️ Technology Stack

-   **R Shiny**: Web application framework
-   **DuckDB**: High-performance analytical database for data processing
-   **Leaflet**: Interactive mapping
-   **Plotly**: Interactive visualizations
-   **DataTables**: Enhanced table display with search and sorting
-   **bslib**: Modern UI components and theming

## 📋 Prerequisites

-   R (version 4.0 or higher)
-   Required R packages (see Installation section)

## 🔧 Installation

1.  **Clone the repository**:

    ``` bash
    git clone https://github.com/amelialouise/shiny-meteorites.git
    cd shiny-meteorites
    ```

2. **Install required R packages**:

    ``` r
    install.packages(c( "shiny", "leaflet", "plotly", "DT", "dplyr", "bslib", "duckdb", "DBI", "jsonlite" ))
    ```

3. **Set up local DuckDB database**:

    ``` r       
    # Download and process NASA meteorite data
    source("R/data.R")
    ```

4. **Build/Launch the app**

# Local development

    ``` bash
    shiny::runApp()
    ```

📁 Project Structure

meteorite-explorer/ ├── app.R \# Main Shiny application ├── data_setup.R \# Data download and DuckDB processing script ├── meteorites.db \# DuckDB database file ├── README.md \# Project documentation └── www/ \# Static assets (if any)

🗃️ Data Processing with DuckDB

The app uses DuckDB for efficient data processing:

```         
Source: NASA meteorite data (JSON format)
Process: Ingest, clean, and transform data using DuckDB SQL
Filter: Focus on meteorites with complete mass and geography info.
Categorize: Create size categories and computed fields
Store: In-memory (local) DuckDB database
```

Benefits:

```         
⚡ Fast query performance
📊 Handles large datasets efficiently
🔄 SQL-based data transformations
💾 No external database setup required!
```

🎨 Customization Themes

The app uses a space-inspired dark theme with warm grays. Modify the CSS in app.R to customize:

tags\$style(HTML(" body { background: linear-gradient(135deg, #6b7280 0%, #4b5563 50%, #374151 100%); color: #f3f4f6; } "))

Data Filters

Add new filtering options by modifying the sidebar inputs and filtered_data() reactive function. Visualizations

Customize map markers, timeline colors, or add new chart types by modifying the render functions. 

🤝 Contributing

```         
Fork the repository
Create a feature branch (git checkout -b feature/new-feature)
Commit your changes (git commit -am 'Add new feature')
Push to the branch (git push origin feature/new-feature)
Create a Pull Request
```

📄 License

This project is licensed under the MIT License - see the LICENSE file for details. 

🙏 Acknowledgments

```         
NASA for providing the comprehensive meteorite landings dataset
The Meteoritical Society for maintaining the official meteorite database
The R Shiny community for excellent documentation and examples
DuckDB team for creating such an efficient embedded database
```

📞 Contact

```         
GitHub: @yourusername
Issues: Report bugs or suggest features
```

Built with ❤️ and R Shiny \| Exploring space rocks, one meteorite at a time! 🌠