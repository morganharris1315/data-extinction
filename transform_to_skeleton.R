# Data-Extinction
# Transform messy Stats NZ income CSV into the skeleton output structure.

library(tidyverse)
library(readr)

# Inputs
skelton_table_2 <- read_csv("Skeleteon Table - Draft Example(2 Income final).csv", show_col_types = FALSE)
raw_data_2 <- read_csv(
  "Data/2 Total personal income_Maori Ethnicity_SA2_2013-2018-2023(in).csv",
  show_col_types = FALSE
)

# Keep an explicit clean ethnicity helper; in this extract Māori appears as M?ori.
raw_data_2 <- raw_data_2 %>%
  mutate(Ethnicity_clean = trimws(as.character(Ethnicity)))

# Filter base rows
base_maori <- raw_data_2 %>%
  mutate(
    year = as.integer(`Census year`),
    sa2_code = as.character(CEN23_GEO_002),
    sa2_name = Area
  ) %>%
  filter(
    year %in% c(2013, 2018, 2023),
    (CEN23_ETH_002 == 2 | Ethnicity_clean == "M?ori"),
    CEN23_AGE_008 == 99,  # Total - age
    CEN23_GEN_002 == 99   # Total - gender
  )

if (nrow(base_maori) == 0) {
  stop(
    "Check source column names/codes in raw_data_2 before continuing."
  )
}

# Numeric values
base_maori <- base_maori %>%
  mutate(obs_value_num = suppressWarnings(as.numeric(OBS_VALUE)))

# 70k+ numerator
# IMPORTANT: R object names cannot start with a number.
inc_70k_levels <- c(
  "$70,001-$100,000",
  "$100,001-$150,000",
  "$150,001-$200,000",
  "$200,001 or more"
)

maori_70k <- base_maori %>%
  filter(`Total personal income` %in% inc_70k_levels) %>%
  group_by(sa2_code, sa2_name, year) %>%
  summarise(maori_70k_plus = sum(obs_value_num, na.rm = TRUE), .groups = "drop")

# Total stated denominator
# Exclude only Not stated / No income stated (per project rule).
not_stated_levels <- c(
  "Not stated",
  "No income stated"
)

maori_total_stated <- base_maori %>%
  filter(
    !(`Total personal income` %in% not_stated_levels),
    `Total personal income` != "Median ($) - total personal income"
  ) %>%
  group_by(sa2_code, sa2_name, year) %>%
  summarise(maori_total_stated = sum(obs_value_num, na.rm = TRUE), .groups = "drop")

# Median income
maori_median <- base_maori %>%
  filter(`Total personal income` == "Median ($) - total personal income") %>%
  transmute(sa2_code, sa2_name, year, maori_median_income = obs_value_num)

# Combine metrics
maori_metrics_long <- maori_70k %>%
  full_join(maori_total_stated, by = c("sa2_code", "sa2_name", "year")) %>%
  full_join(maori_median, by = c("sa2_code", "sa2_name", "year")) %>%
  mutate(
    maori_percent_70kplus = if_else(
      maori_total_stated > 0,
      100 * maori_70k_plus / maori_total_stated,
      NA_real_
    )
  )

# Pivot to wide
maori_metrics_wide <- maori_metrics_long %>%
  pivot_wider(
    id_cols = c(sa2_code, sa2_name),
    names_from = year,
    values_from = c(maori_70k_plus, maori_total_stated, maori_percent_70kplus, maori_median_income),
    names_glue = "{year}_{.value}"
  ) %>%
  rename(
    `SA2 Code` = sa2_code,
    `SA2 Name` = sa2_name
  )

for (yr in c("2013", "2018", "2023")) {
  maori_metrics_wide <- maori_metrics_wide %>%
    rename_with(
      ~ gsub(paste0("^maori_70k_plus_", yr, "$"), paste0(yr, "_maori_70k_plus"), .x),
      everything()
    ) %>%
    rename_with(
      ~ gsub(paste0("^maori_total_stated_", yr, "$"), paste0(yr, "_maori_total_stated"), .x),
      everything()
    ) %>%
    rename_with(
      ~ gsub(paste0("^maori_percent_70kplus_", yr, "$"), paste0(yr, "_maori_percent_70kplus"), .x),
      everything()
    ) %>%
    rename_with(
      ~ gsub(paste0("^maori_median_income_", yr, "$"), paste0(yr, "_maori_median_income"), .x),
      everything()
    )
}

rename_map <- c(
  "2013_maori_70k_plus" = "2013_Maori_70kplus",
  "2013_maori_total_stated" = "2013_Maori_total_stated",
  "2013_maori_percent_70kplus" = "2013_Maori_percent_70kplus",
  "2018_maori_70k_plus" = "2018_Maori_70k_plus",
  "2018_maori_total_stated" = "2018_Maori_total_stated",
  "2018_maori_percent_70kplus" = "2018_Maori_percent_70kplus",
  "2023_maori_70k_plus" = "2023_Maori_70k_plus",
  "2023_maori_total_stated" = "2023_Maori_total_stated",
  "2023_maori_percent_70kplus" = "2023_Maori_percent_70kplus",
  "2013_maori_median_income" = "2013_Maori_median_income",
  "2018_maori_median_income" = "2018_Maori_median_income",
  "2023_maori_median_income" = "2023_Maori_median_income"
)

existing_old_names <- intersect(names(rename_map), names(maori_metrics_wide))
if (length(existing_old_names) > 0) {
  maori_metrics_wide <- maori_metrics_wide %>%
    rename(!!!setNames(existing_old_names, rename_map[existing_old_names]))
}

# Match skeleton schema
# Add any missing skeleton columns as NA so select(all_of()) never fails.
missing_cols <- setdiff(names(skelton_table_2), names(maori_metrics_wide))
for (col in missing_cols) {
  maori_metrics_wide[[col]] <- NA
}

final_tidy <- maori_metrics_wide %>%
  select(all_of(names(skelton_table_2)))

if (nrow(final_tidy) == 0) {
  warning(
    "final_tidy has 0 rows. Check whether input file contains Māori SA2 rows ",
    "for 2013/2018/2023 and whether metric filters match your extract."
  )
}

# Write output
write_csv(final_tidy, "output_income_maori_sa2_tidy.csv", na = "")

# Optional quick checks:
