#2 Lab Coding Task: Summarize the Data:
library("stringr")
library("jsonlite")
library("tidyverse")

#Step 1:
essentia_data_allentown_csv <- read.csv("data/essentia.data.allentown.csv")
essentia_data_csv <- read.csv("data/essentia.data.csv")

check_range <- function(essentia_data_csv, essentia_data_allentown_csv, feature){
  allentown.feature <- essentia_data_allentown_csv[[feature]]
  
  #Step 1 Part 1:
  artist_grouped <- essentia_data_csv |>
    group_by(artist) |>
    
    #Step 2 Part 2:
    summarize(
      min = min(overall_loudness),
      LF = quantile(get(feature), 0.25) - 1.5*IQR(get(feature)),
      UF = quantile(get(feature), 0.75) + 1.5*IQR(get(feature)),
      max = max(get(feature))
    ) |>
    
    #Step 2 Part 3:
    mutate(
      out.of.range = (allentown.feature < min) | 
        (allentown.feature > max)
    ) |>
    mutate(
      unusual = (allentown.feature < LF) | 
        (allentown.feature > UF)
    ) |>
    
    #Step 2 Part 4:
    mutate(
      description = case_when(
        out.of.range ~ "Out of Range",
        unusual ~ "Outlying",
        TRUE ~ "Within Range"
      )
    )
  artist_grouped
}

for (artist in essentia_data_csv) {
  result <- check_range(essentia_data_csv, essentia_data_allentown_csv, feature)
}
result
