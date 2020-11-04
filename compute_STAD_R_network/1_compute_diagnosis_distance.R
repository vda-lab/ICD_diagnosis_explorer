

# ---- Compute distances -----

# Dependencies
require(tidyverse)
require(stad)
require(igraph)
require(usedist)
require(doParallel)


# Definition of distance matrix
new_similarity <- function(v1, v2){
  indices <- which(!is.na(v1) & !is.na(v2) )
  values <- cbind( as.numeric(v1[indices]), as.numeric(v2[indices]) )
  sum(log(1 + 1/apply(values, 1, max)))
}

r01 <- function(x) {
  (x-min(x, na.rm = TRUE))/(max(x)-min(x, na.rm = TRUE))
}


# Read ICD9 Codes
setwd("icd_list")
icd <- read_csv("icd_diagnostic_categories.csv")
names(icd) <- toupper(names(icd))
icd <- icd %>% rename(ICD = ICD_DIAGNOSTIC_CATEGORY, SEQ_NUM = SEQUENCE_NUMBER)

## DIAGNOSES: Exploring the ICD9 for patients with patients
### Read DIAGNOSES_ICD: List of patients and diagnoses
diagnoses <- read_csv("diagnoses.csv")


# Correcting diagnoses
diagnoses <- diagnoses %>% 
  inner_join(icd, by = c("SUBJECT_ID", "HADM_ID", "SEQ_NUM")) 
# Remove ICD
rm(icd)

# Lists of ICD codes
icd_list <- as.data.frame(table(diagnoses$ICD9_CODE)) %>% 
  filter(Freq > 300 & Freq < 4000) %>%
  arrange(Freq)

# as.character(icd_list$Var1)
# icd <- as.character(icd_list$Var1)[1]

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
  
  # MDS
  fit <- cmdscale(distance_matrix, eig=TRUE, k=2)
  mds <- data.frame(x = fit$points[,1], y = fit$points[,2]) %>%
    ggplot(aes(x = x, y = y)) + geom_point(alpha = 0.5)
  ggsave(paste0("a_",icd,"_mds.png"), plot = mds, width = 19, height = 19, units = "cm")
  
  return(paste("Finished", icd))
  
}

# Execution
#lapply( as.character(icd_list$Var1), distance_icd)


# ----- Parallel -------

no_cores <- detectCores() - 1
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
#foreach(i = 1:10) %dopar% print(i)
foreach(i= as.character(icd_list$Var1) ) %dopar% distance_icd(i)
stopCluster(cl)
