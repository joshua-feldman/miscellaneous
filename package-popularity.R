# install if needed
packages <- c("cranlogs","dplyr","tidyr","ggplot2","lubridate","scales")
to_install <- setdiff(packages, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

library(cranlogs)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)

# --- CONFIG ---
# Edit this list
pkgs <- c(
  
  # Heterogeneity Robust Estimators for Staggered Treatment Timing
  "did", "did2s", "didimputation", "DIDmultiplegt", "fixest", "staggered",
  
  # DiD with Covariates
  "DRDID",
  
  # Diagnostics for TWFE with Staggered Timing
  "bacondecomp", "TwoWayFEWeights",
  
  # Diagnostics for TWFE with Staggered Timing
  "honestDiD", "pretrends"
  
) 
# period <- "last-month"  # or use from/to dates below
from <- "2020-01-01"; to <- "2025-09-13"  # uncomment to use explicit dates

# --- FETCH ---
# df <- cran_downloads(packages = pkgs, when = period) %>%
#   mutate(date = as.Date(date))

# If you used from/to instead:
df <- cran_downloads(packages = pkgs, from = from, to = to) %>% mutate(date = as.Date(date))

# Handle packages with zero/NA days gracefully
df <- df %>%
  complete(package, date = seq(min(date), max(date), by = "day"), fill = list(count = 0))

# 7-day rolling average for smoother lines
df <- df %>%
  group_by(package) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(roll7 = zoo::rollapply(count, 7, mean, align = "right", fill = NA_real_)) %>%
  ungroup()

# --- PLOTS ---

# Combined trend (7-day average)
ggplot(df, aes(date, roll7, group = package)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  labs(
    title = "CRAN Downloads (7-day rolling average)",
    subtitle = paste0("Packages: ", paste(pkgs, collapse = ", ")),
    x = NULL, y = "Downloads per day (smoothed)",
    caption = "Source: CRANlogs (RStudio mirror, used as a proxy for total CRAN traffic)."
  ) +
  scale_y_continuous(labels = label_number_si()) +
  theme_minimal(base_size = 12)

# Faceted daily counts (unsmoothed), nice for relative scale within each package
ggplot(df, aes(date, count)) +
  geom_col() +
  facet_wrap(~ package, scales = "free_y") +
  labs(
    title = "CRAN Downloads by Day",
    x = NULL, y = "Downloads per day",
    caption = "Source: CRANlogs"
  ) +
  theme_minimal(base_size = 12)

# --- SUMMARY TABLES ---

# Totals and rank for the chosen period
totals <- df %>%
  group_by(package) %>%
  summarise(total_downloads = sum(count, na.rm = TRUE),
            avg_daily = mean(count, na.rm = TRUE),
            peak_day = max(count, na.rm = TRUE)) %>%
  arrange(desc(total_downloads))

print(totals)

# Monthly breakdown (optional)
monthly <- df %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(package, month) %>%
  summarise(downloads = sum(count, na.rm = TRUE), .groups = "drop") %>%
  arrange(package, month)

print(monthly)
