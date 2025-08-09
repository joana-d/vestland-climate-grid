# A simple things to test vscode + r + radian jazz

library(duckplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

#data <- duckplyr_df_from_csv("C:/Users/joana/VSC_Project/8_Environmental_data/VCG_clean_gridded_daily_climate_2008-2022.csv")

# ==================================== EXPLORING THE DATASET =========================================
#cat("Columns in dataset:\n")
#names(data) |> print()

#n_rows <- data |>
#  summarise(n = n()) |>
#  pull(n)

#cat("\nTotal number of rows:", n_rows, "\n")

#cat("\nVariables:\n")
#data |> distinct(variable) |> collect() |> print()

#cat("\nSiteIDs (first 12):\n")
#data |> distinct(siteID) |> head(12) |> collect() |> print()


# ==================================== TIMESERIES PLOT =========================================
timeseries <- data |>
  filter(siteID == "Alrust", variable == "temperature") |>
  collect() |>
  ggplot(aes(x = date, y = value)) +
  geom_line(color = "steelblue") +
  labs(title = "Temperature over Time at Alrust",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(aspect.ratio = 0.5)  # wider plot

#print(timeseries)


# ==================================== FACETS FOR ONE SITE =========================================
another_plot <- data |>
  filter(siteID == "Alrust") |>
  collect() |>
  ggplot(aes(x = date, y = value)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  labs(title = "Climate Variables over Time at Alrust",
       x = "Date",
       y = "Value")

#print(another_plot)       

# ==================================== YEARLY AVERAGES =========================================

averages <- data |>
  filter(siteID == "Alrust", variable == "temperature") |>
  collect() |>
  mutate(year = year(date)) |>
  group_by(year) |>
  summarise(mean_value = mean(value, na.rm = TRUE)) |>
  ggplot(aes(x = year, y = mean_value)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Yearly Temperature at Alrust",
       x = "Year",
       y = "Mean Temperature (°C)")

#print(averages)

#===================================== HEATMAP ??? =========================================
df_heatmap <- data |>
  filter(variable == "precipitation") |>
  collect() |>
  mutate(
    date = as.Date(date),
    year_month = floor_date(date, "month")  # group by month for cleaner heatmap
  )

heatmap_data <- df_heatmap |>
  group_by(siteID, year_month) |>
  summarise(
    mean_precip = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

plot <- ggplot(heatmap_data, aes(x = year_month, y = reorder(siteID, desc(siteID)), fill = mean_precip)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Precip (mm)", option = "B") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Monthly Average Precipitation by Site",
    x = "Date",
    y = "Site ID"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

#windows(width = 14, height = 5)
#print(plot)

# ==================================== SOME EXPERIMENTING =========================================
# let's see what each sites' plots look like
# disclaimer: a bit messy, but you can see the data 

monthly_climate <- data %>%
  mutate(date = as.Date(date),  # ensure proper date parsing
         date_month = floor_date(date, "month")) %>%
  group_by(date_month, variable, siteID) %>%
  summarise(
    n = n(),
    value = mean(value, na.rm = TRUE),
    sum = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(value = ifelse(variable == "precipitation", sum, value)) %>%
  select(-n, -sum)

# ──────────────────────────────
# some annual climate calcs?
# ──────────────────────────────
annual_climate <- monthly_climate %>%
  mutate(year = year(date_month)) %>%
  group_by(year, variable, siteID) %>%
  summarise(annual_value = mean(value, na.rm = TRUE), .groups = "drop")

# ──────────────────────────────
# data prep for plotting (try wide format?)
# ──────────────────────────────
plot_data <- climate %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(date = as.Date(date)) %>%
  mutate(siteID = factor(siteID))  # You can set levels manually if you want a specific site order

# ──────────────────────────────
# temp plots
# ──────────────────────────────
p <- ggplot(plot_data, aes(x = date, y = temperature, color = siteID)) +
  geom_line() +
  scale_colour_brewer(palette = "Paired") +
  facet_wrap(~ siteID) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Mean Daily Temperature (°C)", x = "Date", y = "Temperature")

#print(p)

# ──────────────────────────────
# relative air moisture
# ──────────────────────────────
air <- p + aes(y = rel_air_moisture) +
  labs(title = "Relative Air Moisture (%)", y = "Relative Moisture")

#print(air)

# ──────────────────────────────
# wind speed
# ──────────────────────────────
wind <- p + aes(y = wind) +
  labs(title = "Wind Speed (m/s)", y = "Wind Speed")

#print(wind)

# ──────────────────────────────
# cloud cover
# ──────────────────────────────
cloud <- p + aes(y = cloud_cover) +
  labs(title = "Cloud Cover", y = "Cloud Cover")

#print(cloud)

# ──────────────────────────────
# precipitation
# ──────────────────────────────
precipittion <- p + aes(y = precipitation) +
  labs(title = "Precipitation (mm)", y = "Precipitation")

  print(precipittion)