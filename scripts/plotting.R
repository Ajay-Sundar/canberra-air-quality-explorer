library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# ============================================================
# 1. LOAD DATA
# ============================================================
df <- read_csv("final_wrangled_dataset_2025_2026.csv")
df$date <- as.Date(df$date)

# ============================================================
# 2. SELECT VARIABLES FOR PCA + CLUSTERING
# ============================================================
vars <- c(
  "aqi_site",
  "pm25_24hr",
  "pm10_24hr",
  "o3_8hr",
  "tavg",
  "wspd",
  "pres"
)

analysis_df <- df %>%
  select(date, station_name, all_of(vars)) %>%
  drop_na()

# ============================================================
# 3. STANDARDIZE VARIABLES
# ============================================================
scaled_matrix <- scale(analysis_df[, vars])

scaled_df <- as.data.frame(scaled_matrix)
scaled_df$date <- analysis_df$date
scaled_df$station_name <- analysis_df$station_name

# ============================================================
# 4. K-MEANS CLUSTERING
# ============================================================
set.seed(123)
k <- 3
km_res <- kmeans(scaled_matrix, centers = k, nstart = 25)

scaled_df$cluster_num <- km_res$cluster

# ============================================================
# 5. PCA
# ============================================================
pca_res <- prcomp(scaled_matrix, center = TRUE, scale. = TRUE)

pca_scores <- as.data.frame(pca_res$x)
pca_scores$date <- analysis_df$date
pca_scores$station_name <- analysis_df$station_name
pca_scores$cluster_num <- km_res$cluster

explained_var <- summary(pca_res)$importance[2, ] * 100
pc1_var <- round(explained_var[1], 2)
pc2_var <- round(explained_var[2], 2)

# ============================================================
# 6. CREATE CLUSTER PROFILES
# ============================================================
cluster_profiles <- scaled_df %>%
  group_by(cluster_num) %>%
  summarise(across(all_of(vars), mean), .groups = "drop")

print(cluster_profiles)

# ============================================================
# 7. ASSIGN MEANINGFUL CLUSTER LABELS
#    Adjust if your cluster order differs
# ============================================================
# Based on your earlier output:
# 2 = Moderate Typical
# 1 = Lower-Wind Elevated AQI
# 3 = Rare Extreme Particulate

cluster_label_map <- c(
  "1" = "Lower-Wind Elevated AQI",
  "2" = "Moderate Typical",
  "3" = "Rare Extreme Particulate"
)

scaled_df$cluster <- factor(
  cluster_label_map[as.character(scaled_df$cluster_num)],
  levels = c(
    "Moderate Typical",
    "Lower-Wind Elevated AQI",
    "Rare Extreme Particulate"
  )
)

pca_scores$cluster <- factor(
  cluster_label_map[as.character(pca_scores$cluster_num)],
  levels = c(
    "Moderate Typical",
    "Lower-Wind Elevated AQI",
    "Rare Extreme Particulate"
  )
)

cluster_profiles$cluster <- factor(
  cluster_label_map[as.character(cluster_profiles$cluster_num)],
  levels = c(
    "Moderate Typical",
    "Lower-Wind Elevated AQI",
    "Rare Extreme Particulate"
  )
)

# ============================================================
# 8. PCA SCATTER PLOT
# ============================================================
ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster, shape = station_name)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "PCA Scatter Plot of Daily Air Quality and Weather Conditions",
    x = paste0("PC1 (", pc1_var, "% variance)"),
    y = paste0("PC2 (", pc2_var, "% variance)"),
    color = "Cluster",
    shape = "Station"
  ) +
  theme_minimal()

# ============================================================
# 9. CLUSTER PROFILE PLOT
# ============================================================
cluster_profiles_long <- cluster_profiles %>%
  select(cluster, all_of(vars)) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variable",
    values_to = "mean_scaled_value"
  )

cluster_profiles_long$variable <- factor(
  cluster_profiles_long$variable,
  levels = vars,
  labels = c(
    "AQI",
    "PM2.5 (24hr)",
    "PM10 (24hr)",
    "O3 (8hr)",
    "Temperature",
    "Wind Speed",
    "Pressure"
  )
)

ggplot(cluster_profiles_long,
       aes(x = variable, y = mean_scaled_value, group = cluster, color = cluster)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Cluster Profile Plot of Standardized Air Quality and Weather Variables",
    x = "Variable",
    y = "Mean Standardized Value",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ============================================================
# 10. CLUSTER COMPOSITION BY STATION
# ============================================================
cluster_station <- pca_scores %>%
  count(station_name, cluster) %>%
  group_by(station_name) %>%
  mutate(prop = n / sum(n))

ggplot(cluster_station, aes(x = station_name, y = prop, fill = cluster)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Cluster Composition by Monitoring Station",
    x = "Station",
    y = "Proportion of Days",
    fill = "Cluster"
  ) +
  theme_minimal()