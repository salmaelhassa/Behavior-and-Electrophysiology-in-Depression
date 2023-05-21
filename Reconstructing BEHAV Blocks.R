library(readxl)
library(dplyr)
library(tidyr)
library(forcats)
library(tidyverse)
library(writexl)

# Set working directory
setwd("/Users/bijankilab-ra4/Desktop/DBSTRD006/New Dataset Reconstruction/Bias Session CSVs/BEHAV")

# Get the list of CSV files
behav.list <- list.files(pattern = "*.csv")

# Initialize empty lists for positive and negative behavior data
pos_behav.list <- list()
neg_behav.list <- list()

# Iterate through the CSV files
for (file in behav.list) {
  data <- read.csv(file)  # Read the CSV file
  
  if (grepl("Post_Neg", file) || grepl("Pre_Neg", file)) {
    neg_behav.list[[file]] <- data  # Add the data to neg_behav.list for negative behavior
  } else if (grepl("Post_Pos", file) || grepl("Pre_Pos", file)) {
    pos_behav.list[[file]] <- data  # Add the data to pos_behav.list for positive behavior
  }
}

# Combine all negative behavior data frames into a single data frame
neg_data <- bind_rows(neg_behav.list, .id = "file")

# Combine all positive behavior data frames into a single data frame
pos_data <- bind_rows(pos_behav.list, .id = "file")

# Extract session information ("Pre" or "Post")
neg_data$session <- ifelse(grepl("Pre", neg_data$file), "Pre", "Post")
pos_data$session <- ifelse(grepl("Pre", pos_data$file), "Pre", "Post")

# Extract run information from file names and remove leading zeros
neg_data$run <- gsub('.*run-0*(\\d+).*', '\\1', neg_data$file)
pos_data$run <- gsub('.*run-0*(\\d+).*', '\\1', pos_data$file)


# Extract date information from file names
neg_data$date <- ifelse(grepl("^EMU-1", neg_data$file), "02/09/2022", ifelse(grepl("^EMU-4", neg_data$file), "02/16/2022", ""))
pos_data$date <- ifelse(grepl("^EMU-1", pos_data$file), "02/09/2022", ifelse(grepl("^EMU-4", pos_data$file), "02/16/2022", ""))

# Extract image number from file names without leading zeros
neg_data$image <- as.integer(gsub('.*_(\\d+).*', '\\1', neg_data$image))
pos_data$image <- as.integer(gsub('.*_(\\d+).*', '\\1', pos_data$image))


# Add group column to indicate "Neg" or "Pos"
neg_data$group <- "Neg"
pos_data$group <- "Pos"

# Remove unnecessary columns
neg_data <- neg_data[, !(names(neg_data) %in% c("file", "labelorder", "RT", "screenshot", "stimulate"))]
pos_data <- pos_data[, !(names(pos_data) %in% c("file", "labelorder", "RT", "screenshot", "stimulate"))]

# Combine negative and positive data frames into a single data frame
DBSTRD006.alltrials.df <- bind_rows(neg_data, pos_data)

# Remove row names
row.names(DBSTRD006.alltrials.df) <- NULL

# Read expected values from Excel file
expected_values <- read_excel("/Users/bijankilab-ra4/Desktop/FIXED Bias_normed_expected_values.xlsx")

# Match up expected values with image number in DBSTRD006.alltrials.df
DBSTRD006.alltrials.df$Expected <- expected_values$Expected[match(DBSTRD006.alltrials.df$imagenumber, expected_values$imagenumber)]

# Rename columns
DBSTRD006.alltrials.df <- DBSTRD006.alltrials.df %>%
  rename(Observed.Rating = rating,
         Intensity = image,
         Valence = group,
         Session.Name = session,
         Date = date,
         Face.Number = imagenumber,
         Run = run,
         Trial = trial)

# Calculate Bias Rating
DBSTRD006.alltrials.df$Bias <- DBSTRD006.alltrials.df$Observed.Rating - DBSTRD006.alltrials.df$Expected

# Rearrange the data frame
DBSTRD006.alltrials.df <- DBSTRD006.alltrials.df %>%
  arrange(factor(Session.Name, levels = c("Pre", "Post")),  # Order Session.Name with "Pre" first, then "Post"
          Run,
          factor(Date, levels = c("02/09/2022", "02/16/2022")))

# Read gamma values from Excel file
gamma_values <- read_excel("/Users/bijankilab-ra4/Desktop/DBSTRD006/Final Datasets/New Final Datasets/DBSTRD006_EPHYS.xlsx")

# Match up gamma values with Session.Name and Trial in DBSTRD006.alltrials.df
DBSTRD006.alltrials.df$Gamma <- gamma_values$Gamma[match(paste(DBSTRD006.alltrials.df$Session.Name, DBSTRD006.alltrials.df$Trial), 
                                                         paste(gamma_values$Session.Name, gamma_values$Trial))]

# Export the final data frame to an Excel file
write_xlsx(DBSTRD006.alltrials.df,
           "/Users/bijankilab-ra4/Desktop/DBSTRD006/Final Datasets/New Final Datasets/DBSTRD006_all_trials.xlsx")
