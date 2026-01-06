# Build a DuckDB database with spatial index from the meteorites parquet file.
library("duckdb")
library("here")

PARQUET_FILE <- here("data", "meteorites.parquet")
DB_FILE <- here("data", "meteorites_spatial.duckdb")

build_spatial_db <- function() {
  cat(strrep("=", 60), "\n")
  cat("Building indexed DuckDB database\n")
  cat(strrep("=", 60), "\n")

  # Check if parquet file exists
  if (!file.exists(PARQUET_FILE)) {
    stop(
      sprintf("Error: %s not found. Run download script first.", PARQUET_FILE),
      call. = FALSE
    )
  }

  # Remove existing database
  if (file.exists(DB_FILE)) {
    cat(sprintf("Removing existing %s...\n", DB_FILE))
    file.remove(DB_FILE)
  }

  # Connect to new database
  cat(sprintf("\nCreating %s...\n", DB_FILE))
  duckdb_drv <- duckdb::duckdb()
  con <- DBI::dbConnect(duckdb_drv, dbdir = DB_FILE, read_only = FALSE)

  DBI::dbExecute(con, "INSTALL spatial;")
  DBI::dbExecute(con, "LOAD spatial;")

  # Load data from parquet and create geometry
  cat("\nLoading data from parquet file and creating geometry...\n")
  load_start <- Sys.time()

  # Load data from parquet and create geometry
  cat("\nLoading data from parquet file and creating geometry...\n")
  load_start <- Sys.time()

  DBI::dbExecute(
    con,
    sprintf(
      "
    CREATE TABLE meteorites AS
    SELECT
      index,
      name,
      catalog_id,
      nametype,
      reclass,
      mass,
      fall,
      -- Fix the year 2101 -> 2010 error
      CASE WHEN catalog_id = 57150 THEN 2010 ELSE year END as year,
      lat,
      lon,
      state,
      county,
      lpi_entry,
      mass / 1000 AS mass_kg,
      mass / 1000000 AS mass_tons,
      CASE
        WHEN mass / 1000000 < 0.001 THEN 'Tiny (< 1kg)'
        WHEN mass / 1000000 < 0.005 THEN 'Small (1-5kg)'
        WHEN mass / 1000000 < 0.02 THEN 'Medium (5-20kg)'
        WHEN mass / 1000000 < 5 THEN 'Large (20kg-5t)'
        ELSE 'Massive (> 5t)'
      END AS size_category,
      CASE
        WHEN catalog_id = 57150 THEN 'Recent (2000+)'  -- Fix era for corrected year
        WHEN year < 1800 THEN 'Ancient (pre-1800)'
        WHEN year < 1900 THEN 'Historical (1800-1899)'
        WHEN year < 1950 THEN 'Early Modern (1900-1949)'
        WHEN year < 2000 THEN 'Late Modern (1950-1999)'
        ELSE 'Recent (2000+)'
      END AS era,
      ST_Point(lon, lat) AS geom
    FROM '%s'
    -- Exclude unknown locations at (0, 0)
    WHERE NOT (lat = 0 AND lon = 0)
    AND NOT (mass = 0)
  ",
      PARQUET_FILE
    )
  )

  load_time <- as.numeric(Sys.time() - load_start)
  cat(sprintf("Data loaded in %.1fs\n", load_time))

  # Count rows
  count <- DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM meteorites")$n
  cat(sprintf("Total meteorites: %s\n", format(count, big.mark = ",")))

  # Create spatial index on geometry
  cat("\nCreating spatial index on geometry column...\n")
  cat("(This may take a few minutes...)\n")
  index_start <- Sys.time()

  DBI::dbExecute(
    con,
    "CREATE INDEX meteorites_geo_idx ON meteorites USING RTREE (geom)"
  )

  index_time <- as.numeric(Sys.time() - index_start)
  cat(sprintf("Spatial index created in %.1fs\n", index_time))

  # Close and check file size
  DBI::dbDisconnect(con, shutdown = TRUE)
  duckdb::duckdb_shutdown(duckdb_drv)

  size_mb <- file.info(DB_FILE)$size / (1024 * 1024)

  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("Database build complete!\n")
  cat(strrep("=", 60), "\n")
  cat(sprintf("Mappable Meteorites: %s\n", format(count, big.mark = ",")))
  cat(sprintf("Database size: %.1f MB\n", size_mb))
  cat("Indexes: geometry (R-tree)\n")
  cat(sprintf("Output: %s\n", DB_FILE))
}

if (sys.nframe() == 0) {
  # Only run if executed directly
  build_spatial_db()
}
