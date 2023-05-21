#----------------------------------------------------------------------------------#
# LOAD XLSX file generated for each patient, select the sessions of interest and do stats
#----------------------------------------------------------------------------------#

# Load libraries
library(readxl)   # For reading Excel files
library(dplyr)    # For data manipulation
library(tidyr)    # For data restructuring
library(stringr)  # For string manipulation
library(writexl)
library(plyr)
library(ggplot2)

# Set the subject ID
subject <- "006"

# Extract the last character of subject
last_char <- substr(subject, nchar(subject), nchar(subject))

# Set the file path
root2read <- paste0("~/Documents/MATLAB/ECoG/neuralData/TRD", last_char, "/")
file_path <- paste0(root2read, "DBSTRD", subject, "_EPHYS.xlsx")

# Read data
df <- read_excel(file_path, sheet = 1)

#Create column called Trial
df$Trial <- rep(1:30, length.out = nrow(df))

# Restructuring data
df$ValencebyDBS <- gsub("PRE", "PRE.", df$ValencebyDBS)   # Add dot after PRE
df$ValencebyDBS <- gsub("POST", "POST.", df$ValencebyDBS) # Add dot after POST
df_new <- separate(df, ValencebyDBS, into = c("Session.Name", "Valence"), sep = "\\.", remove = FALSE)  # Separate ValencebyDBS into Session.Name and Valence columns

# Convert columns to lowercase
df_new$ValencebyDBS <- tolower(df_new$ValencebyDBS)
df_new$Session.Name <- tolower(df_new$Session.Name)
df_new$Valence <- tolower(df_new$Valence)

# Capitalize the first letter of each output column
df_new$ValencebyDBS <- str_to_title(df_new$ValencebyDBS)
df_new$Session.Name <- str_to_title(df_new$Session.Name)
df_new$Valence <- str_to_title(df_new$Valence)

# Export the final data frame to an Excel file
write_xlsx(df_new,
           "/Users/bijankilab-ra4/Desktop/DBSTRD006/Final Datasets/New Final Datasets/DBSTRD006_EPHYS.xlsx")

df_new$depvar=df_new$Gamma
resAVG <- ddply(df_new, c("Valence","Session.Name","ValencebyDBS"), summarise,
                N  = sum(!is.na(depvar)),
                mGamma = mean(depvar, na.rm=TRUE),
                sdGamma  = sd(depvar, na.rm=TRUE),
                seGamma  = sdGamma / sqrt(N))


ggplot(resAVG, aes(x = ValencebyDBS, y = mGamma, fill=Valence)) + 
  geom_bar(stat="summary", fun="mean") + 
  scale_fill_manual(values=c("blue","orange","blue","orange")) + 
  geom_errorbar(aes(ymin= mGamma-seGamma, ymax= mGamma + seGamma), width=0.4) + 
  xlab("") + ylab("Gamma")

#one way anova
model <- aov( Gamma ~ ValencebyDBS, data = df_new)
summary(model)

#n-way anova
model <- aov( Gamma ~ Session.Name * Valence, data = df_new)
summary(model)
