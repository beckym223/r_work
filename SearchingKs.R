library(furrr)   # For parallelized processing
library(dplyr)   # For data manipulation
library(readr)   # For saving CSVs
library(stm)
library(tibble)  # Creating tidy dataframes
library(stm) 
library(progressr)
# Set up parallel execution with 8 cores

results_metadata <- read_csv("~/R_Work/aggregated_results/results_metadata.csv")%>%
  filter(split_on=="paragraph")

get_topic_models <- function(id,
                             seed,
                             starter_ks = c(10,20,40,50,60,70,80)) {
  
  # Set a unique seed for this iteration
  set.seed(seed + as.integer(as.factor(id)))
  
  # Load the saved object for this param_id
  object_path <- results_metadata %>%
    filter(param_id == id) %>%
    pull(file_path)
  
  object <- readRDS(object_path)
  
  results <- object$results
  raw_documents <- results$documents
  stm <- results$dfm2stm
  docs <- stm$documents
  vocab <- stm$vocab
  data <- stm$meta

  
  # Run searchK without specifying cores
  search_output <- searchK(docs, vocab,
                           starter_ks,
                           prevalence=~s(year),
                           data=data)
  
  # Convert results to numeric and add param_id
  kresults <- search_output$results %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(param_id = id)
  
  return(kresults)
}



n_cores = as.integer(availableCores())
plan(multisession, workers = n_cores)

param_ids = results_metadata$param_id
with_progress({
  p <- progressor(steps = length(param_ids))
  results_df <- future_map_dfr(
    param_ids,
    ~ {p()
      Sys.sleep(.2)
      get_topic_models(.x, seed = 632)},  # Pass a base seed
    .options = furrr_options(seed = TRUE)  # Ensures safe parallel RNG
  )
  
})

metadata_to_save <- results_metadata %>%
  select(!c(file_path,last_modified,params_json))
# Merge topic model results with corresponding parameters
final_results <- results_df %>%
  left_join(metadata_to_save,
            by = "param_id")

# Save the merged dataframe as CSV and RDS
write_csv(final_results, "~/R_Work/aggregated_results/topic_model_results_with_params.csv")
