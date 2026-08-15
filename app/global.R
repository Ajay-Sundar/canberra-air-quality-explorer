# ============================================================
# FIT5147 DVP Part 2
# Project: Canberra Air Quality Explorer
# Student: Ajay Sundar Ramanathan
# Student ID: 35208651
#
# global.R
# Loads packages, reads local data files, cleans variables, and
# prepares shared objects used by ui.R and server.R.
# ============================================================


# ---- Packages ----

required_packages <- c(
  "shiny",
  "shinyWidgets",
  "bslib",
  "dplyr",
  "readr",
  "lubridate",
  "ggplot2",
  "plotly",
  "leaflet",
  "scales",
  "tidyr",
  "networkD3",
  "htmlwidgets"
)

missing_packages <- required_packages[
  !(required_packages %in% installed.packages()[, "Package"])
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

lapply(required_packages, library, character.only = TRUE)


# ---- Load Data ----
# All data files are loaded from the local data folder included
# in the submitted code package.

air_data_raw <- read_csv(
  "data/final_wrangled_dataset_2025_2026.csv",
  show_col_types = FALSE
)

npi_data_raw <- read_csv(
  "data/npi_act_cleaned.csv",
  show_col_types = FALSE
)

facility_links_raw <- read_csv(
  "data/station_facility_links_within_10km.csv",
  show_col_types = FALSE
)


# ---- Clean Main Air Quality Dataset ----

air_data <- air_data_raw %>%
  mutate(
    date = as.Date(date, format = "%d-%m-%Y"),
    
    station_name = factor(
      station_name,
      levels = c("Civic", "Florey", "Monash")
    ),
    
    season = factor(
      season,
      levels = c("Summer", "Autumn", "Winter", "Spring")
    ),
    
    aqi_band = factor(
      aqi_band,
      levels = c("Good", "Fair", "Poor", "Very Poor", "Hazardous")
    ),
    
    is_weekend = as.logical(is_weekend),
    weather_matched = as.logical(weather_matched)
  ) %>%
  filter(!is.na(date))


# ---- Clean NPI Facility Dataset ----

npi_data <- npi_data_raw %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(
    facility_name = as.character(facility_name),
    registered_business_name = as.character(registered_business_name),
    primary_anzsic_class_name = as.character(primary_anzsic_class_name),
    suburb = as.character(suburb)
  )


# ---- Clean Station-Facility Link Dataset ----

facility_links <- facility_links_raw %>%
  filter(!is.na(facility_latitude), !is.na(facility_longitude)) %>%
  mutate(
    station_name = factor(
      station_name,
      levels = c("Civic", "Florey", "Monash")
    ),
    within_5km = as.logical(within_5km),
    within_10km = as.logical(within_10km),
    within_5_to_10km = as.logical(within_5_to_10km)
  )


# ---- Shared Filter Choices ----

station_choices <- c("All", levels(air_data$station_name))

season_choices <- c(
  "All",
  "Summer",
  "Autumn",
  "Winter",
  "Spring"
)

pollutant_choices <- c(
  "AQI" = "aqi_site",
  "PM2.5" = "pm25_24hr",
  "PM10" = "pm10_24hr",
  "O3" = "o3_8hr"
)

date_min <- min(air_data$date, na.rm = TRUE)
date_max <- max(air_data$date, na.rm = TRUE)


# ---- Colour Palettes ----

station_palette <- c(
  "Civic" = "#1f78b4",
  "Florey" = "#33a02c",
  "Monash" = "#e31a1c"
)

aqi_band_palette <- c(
  "Good" = "#2ca25f",
  "Fair" = "#ffd60a",
  "Poor" = "#fdae61",
  "Very Poor" = "#d73027",
  "Hazardous" = "#7f0000"
)

season_palette <- c(
  "Summer" = "#fdae61",
  "Autumn" = "#b35806",
  "Winter" = "#74add1",
  "Spring" = "#66bd63"
)


# ---- Station Summary for Map and Insight Panels ----
# This creates one summary row per monitoring station. It is used
# by the Leaflet map and spatial insight panel.

station_summary <- air_data %>%
  group_by(station_name, station_latitude, station_longitude) %>%
  summarise(
    avg_aqi = mean(aqi_site, na.rm = TRUE),
    max_aqi = max(aqi_site, na.rm = TRUE),
    avg_pm25 = mean(pm25_24hr, na.rm = TRUE),
    avg_pm10 = mean(pm10_24hr, na.rm = TRUE),
    facilities_5km = max(npi_facilities_within_5km, na.rm = TRUE),
    facilities_10km = max(npi_facilities_within_10km, na.rm = TRUE),
    nearest_facility = first(na.omit(nearest_facility_name)),
    nearest_facility_distance_km = first(na.omit(nearest_facility_distance_km)),
    .groups = "drop"
  )


# ---- Basic Data Quality Summary ----
# This object documents the final data scope and can be used for
# report writing or future dashboard extensions.

data_quality_summary <- tibble::tibble(
  item = c(
    "Main daily station records",
    "Monitoring stations",
    "Start date",
    "End date",
    "NPI facilities",
    "Station-facility links within 10 km"
  ),
  value = c(
    nrow(air_data),
    length(unique(air_data$station_name)),
    as.character(date_min),
    as.character(date_max),
    nrow(npi_data),
    nrow(facility_links)
  )
)


# ============================================================
# PCA and Cluster Preparation
# ============================================================

# Variables used for multivariate air-quality/weather pattern analysis.
pca_variables <- c(
  "aqi_site",
  "pm25_24hr",
  "pm10_24hr",
  "o3_8hr",
  "tavg",
  "wspd",
  "pres"
)

# Prepare complete records only for PCA.
# This avoids imputation and keeps PCA based only on complete
# daily air-quality/weather records.
pca_input <- air_data %>%
  filter(
    if_all(
      all_of(pca_variables),
      ~ !is.na(.x)
    )
  ) %>%
  select(
    station_name,
    date,
    season,
    all_of(pca_variables)
  )

# Scale numeric variables and calculate PCA.
# Scaling is required because the input variables have different units.
pca_result <- prcomp(
  pca_input %>% select(all_of(pca_variables)),
  center = TRUE,
  scale. = TRUE
)

# Add PCA coordinates back into a plotting dataset.
pca_data <- pca_input %>%
  mutate(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2]
  )

# Use k-means clustering on the first two principal components.
set.seed(5147)

cluster_result <- kmeans(
  pca_data %>% select(PC1, PC2),
  centers = 3,
  nstart = 25
)

pca_data <- pca_data %>%
  mutate(
    cluster_id = factor(cluster_result$cluster)
  )

# Create interpretable cluster names based on observed cluster profiles.
cluster_profiles_raw <- pca_data %>%
  group_by(cluster_id) %>%
  summarise(
    avg_aqi = mean(aqi_site, na.rm = TRUE),
    avg_pm25 = mean(pm25_24hr, na.rm = TRUE),
    avg_pm10 = mean(pm10_24hr, na.rm = TRUE),
    avg_o3 = mean(o3_8hr, na.rm = TRUE),
    avg_wind = mean(wspd, na.rm = TRUE),
    record_count = n(),
    .groups = "drop"
  )

# Typical days are treated as the largest cluster because they
# represent the most common air-quality/weather pattern.
typical_cluster <- cluster_profiles_raw %>%
  arrange(desc(record_count)) %>%
  slice(1) %>%
  pull(cluster_id)

# Extreme particulate days are identified by the highest combined
# particulate matter values.
extreme_particle_cluster <- cluster_profiles_raw %>%
  arrange(desc(avg_pm25 + avg_pm10)) %>%
  slice(1) %>%
  pull(cluster_id)

# The remaining cluster is interpreted as lower-wind elevated-AQI days.
elevated_low_wind_cluster <- cluster_profiles_raw %>%
  filter(
    !(cluster_id %in% c(typical_cluster, extreme_particle_cluster))
  ) %>%
  slice(1) %>%
  pull(cluster_id)

cluster_name_lookup <- tibble(
  cluster_id = factor(c(
    typical_cluster,
    elevated_low_wind_cluster,
    extreme_particle_cluster
  )),
  cluster_name = c(
    "Moderate Typical",
    "Lower-Wind Elevated AQI",
    "Rare Extreme Particulate"
  )
)

pca_data <- pca_data %>%
  left_join(cluster_name_lookup, by = "cluster_id") %>%
  mutate(
    cluster_name = factor(
      cluster_name,
      levels = c(
        "Moderate Typical",
        "Lower-Wind Elevated AQI",
        "Rare Extreme Particulate"
      )
    )
  )

# PCA explained variance for axis labels.
pca_variance <- summary(pca_result)$importance[2, 1:2] * 100

# Colour palette for PCA clusters.
cluster_palette <- c(
  "Moderate Typical" = "#4ade80",
  "Lower-Wind Elevated AQI" = "#f97316",
  "Rare Extreme Particulate" = "#8b5cf6"
)