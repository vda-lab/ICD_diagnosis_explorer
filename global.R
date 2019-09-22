

####################################
# Dependencies
###################################
require(tidyverse)
require(igraph)
require(visNetwork)
require(ggbeeswarm)
require(ggvis)
require(RColorBrewer)
# Shiny dependencies
require(shiny)
require(shinydashboard)
require(dashboardthemes)
require(shinydashboardPlus)
require(shinyWidgets)
require(shinyjs)

#source("collapse.R")
# Working directory (TO REMOVE)
# setwd("/Users/daniel/SynologyDrive/13.PhD/accumulate/code/15_diagnosis/explorer")

####################################
# Read individual files
###################################

# Folder to loead: Distance matrix, Edges list, Nodes list, STAD object
files <- dir("data/icd_list")
r_files <- which(!is.na(files %>% str_extract("a_")))

# Graph layoutL ForceAtlas2
files_layout <- dir("data/layout")

# Combine both files
files_all <- c(files[r_files], files_layout)

# Define status of all files:
  # n: First column is a prefix to detect the interesting files
  # icd: ICD9 code
  # file: file type: distances, edges, mds, etc.
status <- as.data.frame(matrix( unlist(strsplit(files_all, "_")), byrow = TRUE, ncol = 3))
colnames(status) <- c("n", "icd", "file")

# Filter all ICD codes with seven files 
status_s <- status %>% 
  select(-n) %>%
  group_by(icd) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  filter(num == 7)

####################################
# Diagnosis
###################################

# Diagnosis table for all patients with all descriptions and importance order (634709 rows)
diagnosis <- readRDS("data/diagnosis.RDS")
# Diagnosis table for all ICD codes (354 rows)
diagnosis_title <- readRDS("data/diagnosis_title.RDS") %>%
  rename(icd = ICD9_CODE) %>%
  arrange(desc(Freq)) %>%
  inner_join(status_s %>% mutate(icd = as.character(icd)), by = "icd") %>%
  mutate(label = paste0(icd, ": ", LONG_TITLE, " (", Freq ,")") )

# Define icd_choices
icd_choices <- diagnosis_title$icd
names(icd_choices) <- diagnosis_title$label
