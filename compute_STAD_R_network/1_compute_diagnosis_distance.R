

# ---- Compute diagnosis distances -----
# Description: This an example script of how to compute diagnosis distance from the MIMIC-III database.
# The function distance_icd encapsulate the process and save the distance matrix in an RDS object

# Dependencies
require(tidyverse)
require(stad)
require(igraph)
require(usedist)
require(doParallel)


# Function: Diagnosis similarity
new_similarity <- function(v1, v2){
  indices <- which(!is.na(v1) & !is.na(v2) )
  values <- cbind( as.numeric(v1[indices]), as.numeric(v2[indices]) )
  sum(log(1 + 1/apply(values, 1, max)))
}

# Function: Standardize distance between 0 and 1
r01 <- function(x) {
  (x-min(x, na.rm = TRUE))/(max(x)-min(x, na.rm = TRUE))
}

# Read ICD9 Codes
# ----------------------------------------------------------------------------------
# Set the working directory
setwd("icd_list")
# ----------------------------------------------------------------------------------
icd <- read_csv("icd_diagnostic_categories.csv")
names(icd) <- toupper(names(icd))
icd <- icd %>% rename(ICD = ICD_DIAGNOSTIC_CATEGORY, SEQ_NUM = SEQUENCE_NUMBER)

## DIAGNOSES: Exploring the ICD9 for patients with patients
# ----------------------------------------------------------------------------------
### Read DIAGNOSES_ICD: List of patients and diagnoses MIMIC-III database
diagnoses <- "Use a valid function to read the file e.g. read_csv('...')"
# ---------------------------------------------------------------------------------


# Function: Encapsulate computation process in a function to iterate over multiple ICD codes
distance_icd <- function(icd){
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
  
  # Compute distance matrix
  diag_s_values <- diag_s %>% select(-HADM_ID)
  new_sim <- dist_make(diag_s_values, new_similarity, "New similarity (custom)")
  distance_matrix <-  1-r01(new_sim)
  
  # Save RDS
  saveRDS(distance_matrix, file = paste0("a_",icd,"_distance.rds"))
  
  return(paste("Finished", icd))
  
}

# ----- Parallelize the computation -------
no_cores <- detectCores() - 1
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
foreach(i= as.character(icd_list$Var1) ) %dopar% distance_icd(i)
stopCluster(cl)
