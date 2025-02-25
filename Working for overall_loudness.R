#2 Lab Coding Task: Summarize the Data:
library("stringr")
library("jsonlite")
library("tidyverse")

#Step 1:
essentia_data_allentown_csv <- read_csv("data/essentia.data.allentown.csv")
essentia_data_csv <- read_csv("data/essentia.data.csv")
AllenTown_overall_loudness <- essentia_data_allentown_csv[["overall_loudness"]]

#Step 1 Part 1:
artist_grouped <- essentia_data_csv |>
  group_by(artist) |>

#Step 2 Part 2:
  summarize(
    min = min(overall_loudness),
    LF = quantile(overall_loudness, 0.25) - 1.5*IQR(overall_loudness),
    UF = quantile(overall_loudness, 0.75) + 1.5*IQR(overall_loudness),
    max = max(overall_loudness)
  ) |>

#Step 2 Part 3:
  mutate(
    out.of.range = (AllenTown_overall_loudness < min) | 
      (AllenTown_overall_loudness > max)
  ) |>
  mutate(
    unusual = (AllenTown_overall_loudness < LF) | 
           (AllenTown_overall_loudness > UF)
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




  







