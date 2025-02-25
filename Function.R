library("stringr")
library("jsonlite")
library("tidyverse")

# Step 1: Load Data
essentia_data_allentown_csv <- read.csv("data/essentia.data.allentown.csv")
essentia_data_csv <- read.csv("data/essentia.data.csv")

# Define Function
feature <- "overall_loudness"  # Change this to any numeric feature
my_function <- function(essentia_data_csv, essentia_data_allentown_csv, feature) { #Start of Function
allentown_feature <- essentia_data_allentown_csv[[feature]]
  
  # Step 1 Part 1: Group data by artist
  artist_grouped <- essentia_data_csv |>
    group_by(artist) |>
    
    # Step 2 Part 2: Calculate min, max, LF, and UF
    summarize(
      min = min(get(feature), na.rm = TRUE),
      LF = quantile(get(feature), 0.25, na.rm = TRUE) - 1.5 * IQR(get(feature), na.rm = TRUE),
      UF = quantile(get(feature), 0.75, na.rm = TRUE) + 1.5 * IQR(get(feature), na.rm = TRUE),
      max = max(get(feature), na.rm = TRUE)
    ) |>
    
    # Step 2 Part 3: Create 2 new columns, out.of.range and unusal
    mutate(
      out.of.range = (allentown_feature < min) | 
        (allentown_feature > max),
      unusual = (allentown_feature < LF) | 
        (allentown_feature > UF),
      
      # Step 2 Part 4: Create new column, description
      description = case_when(
        out.of.range ~ "Out of Range",
        unusual ~ "Outlying",
        TRUE ~ "Within Range"
      )
    )
} #End of Function

#Step 2: Loop through specific columns of CSV with my function
#Creates Tibble with data for all numeric columns
data_tibble <- tibble()

for (column in colnames(essentia_data_csv)[c(-1,-2,-3,-58,-59,-69,-70)]) {
  result <- my_function(essentia_data_csv, essentia_data_allentown_csv, column) |>
    mutate(feature = column) |>
    select(feature, everything())
  data_tibble <- bind_rows(data_tibble, result)
}

