# Download Meteorite Landings dataset from NASA to local duckdb file.
library("duckdb")
library("dplyr")
library("stringr")
library("here")

# Output file
OUTPUT_FILE <- here("data", "meteorites.parquet")

# Create data directory if it doesn't exist
if (!dir.exists(here("data"))) {
  dir.create(here("data"), recursive = TRUE)
  cat("Created data directory\n")
}

# JSON query with field mapping from data docs
get_meteorites_from_nasa_as_duckdb <- function() {
  cat(strrep("=", 60), "\n")
  cat("Downloading Meteorite Landings from NASA open data portal.\n")
  cat(strrep("=", 60), "\n")

  # Check if file already exists
  if (file.exists(OUTPUT_FILE)) {
    size_mb <- file.info(OUTPUT_FILE)$size / (1024 * 1024)
    cat(sprintf("Warning: %s already exists (%.1f MB)\n", OUTPUT_FILE, size_mb))
    response <- readline(prompt = "Overwrite? (y/n): ")
    if (tolower(response) != 'y') {
      stop("Operation cancelled - file not overwritten.", call. = FALSE)
    }
  }

  # Extract and format meteorites dataset
  meteorites_query <-
    "WITH 
      raw_json AS (
        SELECT * 
        FROM read_json_auto('https://data.nasa.gov/docs/legacy/meteorite_landings/Meteorite_Landings.json')
      ),
      
      raw_json_unnest AS (
        SELECT
          unnest(data, recursive := false) AS values,
          generate_subscripts(data, 1) AS index
        FROM raw_json
      ),
      
      unnested AS (
        SELECT
          index,
          unnest({
            'name': values[9],
            'id': values[10],
            'nametype': values[11],
            'reclass': values[12],
            'mass': values[13],
            'fall': values[14],
            'year': values[15],
            'reclat': values[16],
            'reclong': values[17],
            'States': values[19],
            'Counties': values[20]
          })
        FROM raw_json_unnest
      ),
      
    formatted AS (
      SELECT 
        index::INTEGER AS index,
        TRIM(BOTH '\"' FROM name::VARCHAR) AS name, 
        id::INTEGER AS id, 
        TRIM(BOTH '\"' FROM nametype::VARCHAR) AS nametype, 
        TRIM(BOTH '\"' FROM reclass::VARCHAR) AS reclass, 
        mass::DECIMAL AS mass, 
        TRIM(BOTH '\"' FROM fall::VARCHAR) AS fall, 
        year::DATE AS year,
        reclat::DECIMAL AS lat,
        reclong::DECIMAL AS lon,
        TRIM(BOTH '\"' FROM States::VARCHAR) AS state,
        TRIM(BOTH '\"' FROM Counties::VARCHAR) AS county
      FROM unnested
    )

    SELECT 
      index, 
      name, 
      id AS catalog_id, 
      nametype, 
      reclass, 
      mass, 
      fall,
      EXTRACT(YEAR FROM year) AS year, 
      lat, 
      lon, 
      state, 
      county,
      'https://www.lpi.usra.edu/meteor/metbull.php?code=' || id AS lpi_entry
    FROM formatted
    WHERE lat IS NOT NULL 
      AND mass IS NOT NULL;
    "

  # Connect to DuckDB
  cat("Initializing DuckDB...\n")
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cat(strrep("=", 60), "\n")
  cat("Fetching data from NASA API - this can take 5-10 minutes!\n")
  cat("Note: DuckDB's progress bar has limited support in R.\n")
  cat("The download is working even if no progress or only 0% shows!\n")
  cat(strrep("=", 60), "\n\n")
  flush.console()

  query_start <- Sys.time()
  res <- DBI::dbGetQuery(con, meteorites_query)
  query_end <- Sys.time()
  query_runtime <- as.numeric(query_end - query_start)

  # Display results info
  cat(sprintf(
    "Done!\nLoaded %s meteorites in %.2f seconds\n",
    format(nrow(res), big.mark = ","),
    query_runtime
  ))

  # Save to parquet
  cat(sprintf("\nSaving to %s...\n", OUTPUT_FILE))
  arrow::write_parquet(res, OUTPUT_FILE, compression = "zstd")

  # Check file size
  size_mb <- file.info(OUTPUT_FILE)$size / (1024 * 1024)
  count <- nrow(res)

  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("Save complete!\n")
  cat(strrep("=", 60), "\n")
  cat(sprintf("Meteorites: %s\n", format(count, big.mark = ",")))
  cat(sprintf("File size: %.1f MB\n", size_mb))
  cat(sprintf("Download time: %.1fs\n", query_runtime))
  cat(sprintf("Output: %s\n", OUTPUT_FILE))

  # Cleanup
  DBI::dbDisconnect(con, shutdown = TRUE)
  rm(res)
  invisible(gc()) # free memory!
}

if (sys.nframe() == 0) {
  # Only run if executed directly
  get_meteorites_from_nasa_as_duckdb()
}
