# Script to transform messy CSV to skeleton table format
# This script restructures data to match the Stats NZ SDMX-compatible format

library(tidyverse)
library(readr)

# Read your messy CSV file
messy_data <- read_csv("Skeleteon Table - Draft Example(2 Income final).csv")

# Read your skeleton template (optional, for reference)
skeleton_template <- read_csv("skeleton_table.csv")

# Transform the data to match skeleton structure
transformed_data <- messy_data %>%
  # Pivot longer to convert year columns to rows
  pivot_longer(
    cols = starts_with("201") | starts_with("202"),
    names_to = "year_metric",
    values_to = "value"
  ) %>%
  # Separate year and metric information
  separate(year_metric, 
           into = c("year", "ethnicity", "metric"), 
           sep = "_", 
           extra = "merge") %>%
  # Create the SDMX-compatible structure
  mutate(
    STRUCTURE = "CEN23_INC_004",
    STRUCTURE_ID = "STATSNZ:CEN23_INC_004(1.0)",
    STRUCTURE_NAME = "Total personal income, ethnicity, age (life cycle groups), and gender for the census usually resident population count aged 15 years and over",
    ACTION = "I",
    CEN23_YEAR_001 = as.integer(year),
    CEN23_GEO_002 = as.integer(SA2_Code),
    CEN23_GEO_NAME = SA2_Name,
    CEN23_TOI_002 = NA,  # Populate with appropriate income category code
    CEN23_ETH_002 = case_when(
      ethnicity == "Maori" ~ 2,
      TRUE ~ 999  # 999 for Total
    ),
    CEN23_AGE_008 = 99,  # Total - age
    CEN23_GEN_002 = 99,  # Total - gender
    OBS_VALUE = as.integer(value),
    OBS_STATUS = NA  # Add observation status if available
  ) %>%
  # Select and reorder columns to match skeleton
  select(
    STRUCTURE,
    STRUCTURE_ID,
    STRUCTURE_NAME,
    ACTION,
    CEN23_YEAR_001,
    CEN23_GEO_002,
    CEN23_GEO_NAME,
    CEN23_TOI_002,
    CEN23_ETH_002,
    CEN23_AGE_008,
    CEN23_GEN_002,
    OBS_VALUE,
    OBS_STATUS
  )

# Write the transformed data
write_csv(transformed_data, "transformed_data.csv", na = "")

# Display first few rows to verify
head(transformed_data)
print("Transformation complete! Check 'transformed_data.csv'")
