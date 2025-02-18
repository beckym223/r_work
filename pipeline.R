library(jsonlite)
library(tibble)
library(fs)
library(digest)

# Function to generate a unique hash-based param_id
generate_param_id <- function(params) {
  param_str <- paste0(names(params), "=", unlist(params), collapse = "_")
  param_hash <- digest(param_str, algo = "md5")
  return(paste0("params_", substr(param_hash, 1, 8)))  # Shorten to 8 chars
}
# Load the function

# Read JSON file with parameter sets
param_sets <-fromJSON("~/R_Work/params.json",simplifyVector = FALSE)


# Load your data (replace with actual data loading step)
data <- read.csv("~/R_Work/data.csv")

# Create an empty list to store results
results_list <- list()
metadata <- list()  # Metadata storage
source("~/R_Work/processing.R")
i = 16
# Iterate over each set of parameters
for (i in seq_along(param_sets)) {
  print(i)
  params <- param_sets[[i]] # Extract the parameter set
  # Generate a hash-based ID
  
  param_id <- generate_param_id(params)
  
  # Run the function
  result <-create_corpus(data, params = params)
  
  # Define file path
  file_path <- paste0("~/R_Work/results/results_", param_id, ".rds")
  
  # Save results
  saveRDS(list(parameters = params, results = result), file_path)
  
  # Get file metadata
  file_info <- file_info(file_path)
  
  # Store metadata entry
  metadata[[i]] <- tibble::tibble(
    param_id = param_id,
    file_path = file_path,
    last_modified = file_info$modification_time,
    params_json = jsonlite::toJSON(params, auto_unbox = TRUE)%>%as.character(),
    !!!as.list(params)  # Spread params into separate columns
  )
  
  # Store results in master list
  results_list[[param_id]] <- list(parameters = params, results = result)
}

# Save all results in a single RDS file
saveRDS(results_list, "~/R_Work/results/all_text_processing_results.rds")

# Save metadata as CSV
metadata_df <- dplyr::bind_rows(metadata)
write.csv(metadata_df, "~/R_Work/results/results_metadata.csv", row.names = FALSE)
