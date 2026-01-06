initialize_database <- function(con, source_db, table) {
  source_con <- dbConnect(duckdb::duckdb(), dbdir = source_db, read_only = TRUE)
  meteorites <- dbReadTable(source_con, table)
  dbDisconnect(source_con)

  dbWriteTable(con, table, meteorites)
}

# Smaller dataset strategy?
con <- DBI::dbConnect(
  duckdb::duckdb(),
  "data/meteorites.duckdb",
  read_only = TRUE
)

# Create summary stats for display
total_in_db <-
  tbl(con, "meteorites") %>%
  collect() %>%
  nrow()

displayed_count <- 42

# Smart data loading strategy for 38k+ meteorites
load_meteorite_data <- function() {
  # Load different tiers of interesting meteorites

  # Tier 1: Large meteorites (top 1% by mass)
  large_meteorites <- DBI::dbGetQuery(
    con,
    "
    SELECT * FROM meteorites 
    WHERE mass >= (SELECT PERCENTILE_CONT(0.99) WITHIN GROUP (ORDER BY mass) FROM meteorites WHERE mass > 0)
    AND mass IS NOT NULL
  "
  )

  # Tier 2: Historical meteorites (pre-1950)
  historical_meteorites <- DBI::dbGetQuery(
    con,
    "
    SELECT * FROM meteorites 
    WHERE year < 1950 AND year IS NOT NULL AND mass IS NOT NULL
    AND mass >= 100  -- At least 100g to be interesting
  "
  )

  # Tier 3: Recent discoveries with decent size (random sample)
  recent_meteorites <- DBI::dbGetQuery(
    con,
    "
    SELECT * FROM meteorites 
    WHERE year >= 1950 AND mass >= 1000 AND year IS NOT NULL AND mass IS NOT NULL
    ORDER BY RANDOM() 
    LIMIT 2000
  "
  )

  # Combine and deduplicate
  all_selected <- rbind(
    large_meteorites,
    historical_meteorites,
    recent_meteorites
  ) %>%
    distinct(name, .keep_all = TRUE) %>%
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

  return(all_selected)
}

meteorites_sm <- load_meteorite_data()
DBI::dbDisconnect(con)
