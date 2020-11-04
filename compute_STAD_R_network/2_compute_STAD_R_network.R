

# ---- Compute STAD-R network -----
# Description: This an example script of how to compute STAD-R for multiple distance matrices

# Dependencies
require(tidyverse)
require(stad)
require(igraph)
require(usedist)
require(doParallel)

# ----------------------------------------------------------------------------------
# Set the working directory
setwd("icd_list")
# ----------------------------------------------------------------------------------


# Snippet to determine the number of pending codes:
# The data.frame status_to contains the list of ICD codes to execute.
# It reads the files from the working directory.

# Read files in directory
files <- dir()
r_files <- which(!is.na(files %>% str_extract("a_")))
# Define status
status <- as.data.frame(matrix( unlist(strsplit(files[r_files], "_")), byrow = TRUE, ncol = 3))
colnames(status) <- c("n", "icd", "file")
status_s <- status %>% 
  select(-n) %>%
  group_by(icd) %>%
  summarise(num = n()) %>%
  ungroup()
# ICD to analyze
status_todo <- status_s %>% filter(num == 1)


# Read ICD9 Codes
icd <- read_csv("icd_diagnostic_categories.csv")
names(icd) <- toupper(names(icd))
icd <- icd %>% rename(ICD = ICD_DIAGNOSTIC_CATEGORY, SEQ_NUM = SEQUENCE_NUMBER)

## DIAGNOSES: Exploring the ICD9 for patients with patients
# ----------------------------------------------------------------------------------
### Read DIAGNOSES_ICD: List of patients and diagnoses MIMIC-III database
diagnoses <- "Use a valid function to read the file e.g. read_csv('...')"
# ---------------------------------------------------------------------------------


# Function: Encapsulate computation process in a function to iterate over multiple distance matrices
stad_diagnosis <- function(icd){
  
  require(tidyverse)
  require(stad)
  require(igraph)
  require(usedist)
  require(doParallel)
  
  # Filter values
  diag <- diagnoses %>% filter(ICD9_CODE == icd)
  
  # Remove duplicates
  diag <- diag %>% 
    group_by(HADM_ID) %>%
    mutate(MIN_SEQ = min(SEQ_NUM)) %>%
    filter(MIN_SEQ == SEQ_NUM) %>%
    select(-MIN_SEQ) %>%
    ungroup()
  
  # Spread diagnosis
  diag_s <- diagnoses %>%
    inner_join(diag %>% select(HADM_ID), by = "HADM_ID") %>%
    group_by(HADM_ID, ICD) %>%
    summarize(N = min(SEQ_NUM) ) %>% 
    ungroup() %>%
    spread(ICD, N, fill = NA)
  
  # Select IDs to use in nodes
  diag_s_values <- diag_s %>% select(-HADM_ID)
  diag_s_id <- diag_s %>% select(HADM_ID)
  
  # Ranking
  ranking <- diag_s %>%
    select(HADM_ID) %>% 
    inner_join(diag %>% select(HADM_ID, SEQ_NUM), by = "HADM_ID")
  
  # STAD
  distance_matrix <- readRDS(paste0("a_",icd,"_distance.rds"))
  set.seed(42)
  diag_stad <- stad(distance_matrix)
  saveRDS(diag_stad, file = paste0("a_",icd,"_stad.rds"))
  
  # Labels
  diag_labels <- diagnoses %>%
    filter(ICD9_CODE == icd) %>%
    group_by(HADM_ID) %>%
    summarise(LABEL = paste(ICD, collapse = "-") ) %>%
    ungroup()
  
  ## Exporting graph
  net_stad <- diag_stad$graph
  
  # Edges
  net_stad_edges <- as_data_frame(net_stad) %>%
    mutate(Id = row_number(),
           value = 1/(1+value)) %>%
    select(Id, from, to, value) %>%
    rename(Source = from, Target = to, Weight = value)
  
  # Nodes
  net_stad_vertices <- ranking %>%
    inner_join(diag_labels, by = "HADM_ID") %>%
    mutate(Id = row_number()) %>%
    select(Id, LABEL, SEQ_NUM)
  
  # Save
  write_csv(net_stad_vertices, paste0("a_", icd, "_nodes.csv"))
  write_csv(net_stad_edges, paste0("a_", icd, "_edges.csv"))
  
  return(paste("Finished", icd))
}


# ----- Parallelize the computation -------
no_cores <- detectCores() - 1
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
foreach(i= as.character(status_todo$icd) ) %dopar% stad_diagnosis(i)
stopCluster(cl)