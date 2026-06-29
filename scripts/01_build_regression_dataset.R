library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(rnaturalearth)
library(sf)

source("~/Symobio/Scripts_C/diagnostics/01_build_country_diagnostics.R")

# ---------------------------------------------------------
# Directories
# ---------------------------------------------------------

dir_project <- "~/Symobio/BF_NEE_Diagnostics_Website"
dir_bf      <- "~/Symobio/Data/Processed/BF_NEE_Countries"
dir_diag    <- "~/Symobio/Data/Processed/BF_NEE_Diagnostics"

dir_out <- file.path(dir_project, "data")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# Load valid countries
# ---------------------------------------------------------

valid <- read_csv(
  file.path(dir_diag, "bfnee_valid_countries.csv"),
  show_col_types = FALSE
)

valid_iso3 <- valid %>%
  filter(valid_for_analysis) %>%
  pull(iso3)

message("Valid countries: ", length(valid_iso3))

# ---------------------------------------------------------
# Read BF-NEE summary per country
# ---------------------------------------------------------

read_bfnee_summary <- function(iso3) {
  
  iso3_lower <- tolower(iso3)
  
  f <- file.path(
    dir_bf,
    iso3,
    paste0("BF_NEE_summary_", iso3_lower, ".csv")
  )
  
  if (!file.exists(f)) {
    warning("Missing BF-NEE summary for ", iso3)
    return(NULL)
  }
  
  x <- read_csv(f, show_col_types = FALSE)
  
  x %>%
    mutate(
      iso3 = iso3,
      bfnee_sum_km2 = BF_NEE,
      natural_loss_sum_km2 = total_natural_loss_area,
      weight_sum = total_weight_sum_S_times_P
    ) %>%
    select(
      iso3,
      country_name,
      transition,
      year,
      n_natural_classes,
      natural_loss_sum_km2,
      weight_sum,
      bfnee_sum_km2
    )
}

bfnee_df <- bind_rows(
  lapply(valid_iso3, read_bfnee_summary)
)

# ---------------------------------------------------------
# Add world region / continent
# ---------------------------------------------------------

world_sf <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

world_regions <- world_sf %>%
  st_drop_geometry() %>%
  mutate(
    iso3_pipeline = ifelse(
      is.na(iso_a3) | iso_a3 == "-99",
      adm0_a3,
      iso_a3
    )
  ) %>%
  transmute(
    iso3 = iso3_pipeline,
    continent,
    region_un = region_un,
    subregion = subregion
  ) %>%
  filter(
    !is.na(iso3),
    iso3 != "-99",
    iso3 != "ATA"
  )

bfnee_df <- bfnee_df %>%
  left_join(world_regions, by = "iso3")

# ---------------------------------------------------------
# Derived variables for regression
# ---------------------------------------------------------

regression_df <- bfnee_df %>%
  mutate(
    bfnee_per_loss = bfnee_sum_km2 / natural_loss_sum_km2,
    log_bfnee = log1p(bfnee_sum_km2),
    log_natural_loss = log1p(natural_loss_sum_km2),
    log_weight_sum = log1p(weight_sum)
  ) %>%
  arrange(continent, iso3, year)

# ---------------------------------------------------------
# Save
# ---------------------------------------------------------

write_csv(
  regression_df,
  file.path(dir_out, "bfnee_regression_dataset.csv")
)

message("Saved regression dataset:")
message(file.path(dir_out, "bfnee_regression_dataset.csv"))

# ---------------------------------------------------------
# Quick checks
# ---------------------------------------------------------

print(
  regression_df %>%
    count(continent, year)
)

print(
  regression_df %>%
    summarise(
      n_rows = n(),
      n_countries = n_distinct(iso3),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      total_bfnee = sum(bfnee_sum_km2, na.rm = TRUE),
      total_natural_loss = sum(natural_loss_sum_km2, na.rm = TRUE)
    )
)


