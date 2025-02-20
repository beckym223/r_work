library(furrr)   # For parallelized processing
library(dplyr)   # For data manipulation
library(readr)   # For saving CSVs
library(stm)
library(tibble)  # Creating tidy dataframes
library(stm) 
library(progressr)
library(purrr)
# Set up parallel execution with 8 cores

full_metadata = read_csv("~/R_Work/aggregated_results/results_metadata.csv",show_col_types=FALSE)
results_metadata <- full_metadata%>%
  filter(split_on=="paragraph") %>%
  select(param_id,file_path)%>%
  collect()

get_topic_models <- function(id,
                             seed,
                             starter_ks = c(10,20,40,50,60,70,80),
                             cores = availableCores() - 1,
                             p = NULL) {  # Pass progressor
  # For parallelized processing
  
  start_time <- Sys.time()
  set.seed(seed + as.integer(as.factor(id)))
  
  object_path <- results_metadata %>%
    filter(param_id == id) %>%
    pull(file_path)
  
  object <- readRDS(object_path)
  
  results <- object$results
  stm <- results$dfm2stm
  docs <- stm$documents
  vocab <- stm$vocab
  data <- stm$meta
  
  # Update Progress Message
  if (!is.null(p)) {
    p(sprintf("Starting searchK for param_id: %s with %d cores", id, cores))
  }
  
  # Run searchK with multiple cores
  search_output <- searchK(docs, vocab, starter_ks,
                           prevalence = ~s(year),
                           data = data,
                           cores = cores
                           )
  
  end_time <- Sys.time()
  runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Update Progress Message After Completion
  if (!is.null(p)) {
    p(sprintf("Finished param_id: %s in %.2f seconds", id, runtime))
  }
  
  kresults <- search_output$results %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(param_id = id, runtime_seconds = runtime)
  
  return(kresults)
}

run_with_multiprocess<- function(param_ids,
                                 max_cores_to_use,
                                 perc_for_workers){
  start_time = Sys.time()
  worker_cores = floor(max_cores_to_use * min(c(1,perc_for_workers)))
  leftover_cores = floor(max_cores_to_use/worker_cores)
  plan(multisession, workers = worker_cores)
  
  with_progress({
    p <- progressor(along = param_ids)  # Create a progressor
    results <- future_map(
      param_ids,
      ~ get_topic_models(.x, seed = 632,cores=leftover_cores),  # Pass progressor
      .options = furrr_options(seed = TRUE, globals = c("get_topic_models", "results_metadata","p","leftover_cores"))
    )
  end_time = Sys.time()
  runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))
  p(sprintf("Finished run with: %d workers and %d for searchK of %d params in %.2f seconds", worker_cores, leftover_cores, length(param_ids), runtime))
  
  })
  
  
  results_df = bind_rows(results)
  return(results_df)
  
}

n_cores = as.integer(availableCores())
start_time = Sys.time()
#worker_cores = 0
# plan(multisession, workers = worker_cores)

param_ids = results_metadata$param_id %>% head(4)
with_progress({
  p <- progressor(along = param_ids)  # Create a progressor
  results <- map(
    param_ids,
    ~ get_topic_models(.x, seed = 632,p=p,cores=8),  # Pass progressor
    #.options = furrr_options(seed = TRUE, globals = c("get_topic_models", "results_metadata","p"))
  )
})
end_time = Sys.time()
runtime1 <- as.numeric(difftime(end_time, start_time, units = "secs"))
#print(sprintf("Finished run with: %d workers and %d for searchK of %d params in %.2f seconds", worker_cores, leftover_cores, length(param_ids), runtime))

results_df = bind_rows(results)
metadata_to_save <- full_metadata %>%
  select(!c(file_path,last_modified,params_json))
# Merge topic model results with corresponding parameters
final_results <- results_df %>%
  left_join(metadata_to_save,
            by = "param_id")

# Save the merged dataframe as CSV and RDS
write_csv(final_results, "~/R_Work/aggregated_results/topic_model_results_with_params1.csv")
