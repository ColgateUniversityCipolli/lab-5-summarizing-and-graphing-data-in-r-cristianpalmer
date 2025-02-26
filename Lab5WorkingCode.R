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
data_tibble_everything <- tibble()

for (column in colnames(essentia_data_csv)[c(-1,-2,-3,-58,-59,-69,-70)]) {
  result <- my_function(essentia_data_csv, essentia_data_allentown_csv, column) |>
    mutate(feature = column) |>
    select(feature, everything())
  data_tibble_everything <- bind_rows(data_tibble, result)
}
######################################
#Tibble With Specific Features Chosen#
#Chose features where only one band was in range while the other 2 were in range
######################################
data_tibble_specific <- tibble()

for (column in colnames(essentia_data_csv)[c(12,13,15,17,19,23,24,39,43,45)]) {
  result_specific <- my_function(essentia_data_csv, essentia_data_allentown_csv, column) |>
    mutate(feature = column) |>
    select(feature, everything())
  data_tibble_specific <- bind_rows(data_tibble_specific, result_specific)
}
#Step 3: This is the data which I used for the table which I made in the document titled "LatexTable.Rnw"
######################################
#Select Specific Columns to Use for Xtable
######################################
data_selected_columns <- data_tibble_specific |>
  select(feature, artist, out.of.range, unusual, description)

#Step 4:
#Used Shiny App To Create Graphs#


#Graph 1#
####################################
# Load Data
####################################
dat <- read_csv("data_tibble_specific.csv")
####################################
# Mutate data for plot
####################################
df <- dat %>%
  dplyr::select("description", "artist") %>%
  drop_na() %>%
  group_by(!!sym("description"), !!sym("artist")) %>%
  summarise(Observations = sum(!is.na(!!sym("description"))), .groups = "drop") %>%
  tidyr::complete(!!sym("description"), !!sym("artist")) %>%
  replace_na(list(Observations = 0)) %>%
  group_by(!!sym("artist")) %>%
  mutate(Proportion = Observations / sum(Observations)) %>%
  arrange(desc(!!sym("description"))) %>%
  mutate(Percent = Proportion * 100) %>%
  mutate(denoted.group = paste("artist", " = ", !!sym("artist"), sep = ""))
####################################
# Create Plot
####################################
Graph_1 <- ggplot(df, aes(x = !!sym("description"), y = Proportion)) +
  geom_bar(stat = "identity", width = 0.5, fill = "darkred") +
  get("theme_bw")() +
  xlab("Description") +
  ylab(ifelse("Proportion" == "", "Proportion", "Proportion")) +
  ggtitle("Proportion of Each Band's Description  (Out of Range,  Outlying,  or  Within Range)  for Selected Features", "") +
  geom_hline(yintercept = 0) +
  facet_wrap(~denoted.group)
####################################
# Print Plot
####################################
Graph_1
####################################
# Summarize Data
####################################
dat.summary <- dat %>%
  dplyr::select("artist", "description") %>%
  group_by(!!sym("artist"), !!sym("description")) %>%
  summarize(Observations = sum(!is.na(!!sym("description"))), .groups = "drop") %>%
  tidyr::complete(!!sym("artist"), !!sym("description")) %>%
  replace_na(list(Observations = 0))
missing.obs <- dat %>%
  summarize(missing = sum(is.na(!!sym("description")) | is.na(!!sym("artist")))) %>%
  pull(missing)
dat.summary <- dat.summary %>%
  filter(!(is.na(!!sym("description")) | is.na(!!sym("artist")))) %>%
  group_by(!!sym("artist")) %>%
  mutate(Proportion = Observations / sum(Observations)) %>%
  arrange(desc(!!sym("artist"))) %>%
  arrange(!!sym("description"), .by_group = TRUE) %>%
  mutate(Percent = Proportion * 100) %>%
  mutate_if(is.numeric, round, 4) %>%
  ungroup() %>%
  add_row(`:=`(!!sym("artist"), "Rows with Missing Data"), `:=`(!!sym("description"), NA), Observations = missing.obs, Proportion = NA, Percent = NA)
####################################
