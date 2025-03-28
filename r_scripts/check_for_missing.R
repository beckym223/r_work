options(warn = -1)
#source('env.R')
library(optparse)
library(digest)
library(quanteda)
library(stm)
library(tidyverse)
library(logr)
library(reshape2)

setwd("~/stm_work")
source("r_scripts/get_processed_docs.R")
source('r_scripts/formats.R')

mkdir<-function(dirname){
  if (!dir.exists(dirname)) {
    dir.create(dirname, recursive = TRUE, showWarnings = FALSE)
  }
}

default_output="results_3_20_spline"
default_log_path = file.path(default_output,"logs","missing_stms.log")
default_df = file.path(default_output,"missing.csv")
  option_list = list(
      
      make_option(c("-o", "--output_dir"), action='store',type = "character", default = default_output,
                  help = "Directory to save results"),
      
      make_option(c('-l','--logs'),type='character',help="path to log file",default=default_log_path),
      make_option(c('-d',"--dataframe"),type='character',help="path to data",default=default_df),
      
      make_option(c('-i','--index'),type='integer',default=1,help='Index of thing to check')
      
    )
    
  
  # Parse options
  options <- parse_args(OptionParser(option_list = option_list))
log_open(options$logs,logdir=F,compact=T,show_notes=F)
data<-read_csv(options$dataframe,show_col_types=F)

opt<-c(options,as.list(data[options$index,]))
log_print(sprintf("Working with: %s at index %d",opt$model_id,opt$index))
  
  # Construct file paths
  doc_vocab_id <- get_doc_id.opt(opt)
  model_id <- opt$model_id

  plot_dir<-get_plot_data_dir(opt$output_dir)
  topic_dir<-get_topic_data_dir(opt$output_dir)
  model_dir<-get_model_save_dir(opt$output_dir)
  
  mkdir(plot_dir)
  mkdir(topic_dir)
  mkdir(model_dir)
  
  model_out_path <- get_model_out_path(opt$output_dir,model_id)
  stm_fitted<-file.exists(model_out_path)
  
  if (stm_fitted){
    stop(sprintf("STM fitted at %s. Stopping.",model_out_path))
  }

  plot_path = file.path(plot_dir,get_plot_data_path(model_id))
  topic_path = file.path(topic_dir,get_topic_data_path(model_id))

  prepped_docs_path<-get_prepped_docs_path.opt(opt)
  output = list()
  if (file.exists(prepped_docs_path)){
    log_print("Loading preprocessed documents...")
    output <- readRDS(prepped_docs_path)
  }
  if (is.null(output$processed)){
    log_print("(re)prepping documents...")
    output <- get_processed_docs(opt$unit,
                                 opt$text_truncation,
                                 opt$ngram,
                                 opt$lower_thresh,
                                 opt$output_dir
                                 )
    log_print(glue::glue("Saved processed documents to {prepped_docs_path}"))
  }
  processed<- output$processed
  docs<-processed$documents
  vocab<-processed$vocab
  meta<-processed$meta
  # Check if enough documents are available for STM
  if (length(processed$documents) < 100) {
    stop(glue::glue("Too few documents ({length(processed$documents)}) to proceed with STM. Exiting."))
    }
    

    log_print(glue::glue("Starting STM fitting with {opt$num_topics} topics..."))
    stm_start_time <- Sys.time()
    stm_obj <- stm(docs,vocab, K = opt$num_topics, init.type = "Spectral",
                   prevalence = ~s(meeting_num),emtol=0.0001,
                   data = meta,
                   verbose = T
                   )
    
    stm_end_time <- Sys.time()
    log_print(glue::glue("STM model fitting completed in {difftime(stm_end_time, stm_start_time, units='mins')} minutes."))
    
    # Ensure output directory exists
    if (!dir.exists(opt$output_dir)) {
      log_print(glue::glue("Creating output directory: {opt$output_dir}"))
      mkdir(opt$output_dir)
      }
    
    # Save model
    log_print(glue::glue("Saving stm to {model_out_path}"))
    saveRDS(stm_obj, file = model_out_path)
    log_print(glue::glue("STM model saved to {model_out_path}"))
    

    
  # get output paths for stm data


    log_print("Processing stm")
    
    source('r_scripts/get_stm_data_func.R')
    
    topics <- seq_len(stm_obj$settings$dim$K)
    prep <- estimateEffect(topics ~ meeting_num, stmobj =stm_obj, metadata = meta)
    p<-plot(prep,'meeting_num',method = 'continuous',omit.plot = T)
    s<-summary(prep)
    
    out<-get_stm_data(stm_obj, docs, meta, p, s, doc_vocab_id, model_id)
    
    topic_df<-out$topic_df
    plot_df<- out$plot_df
    
    log_print(glue::glue("Saving topic_df to {topic_path}"))
    write_csv(topic_df,topic_path)
    
    log_print(glue::glue("Saving plot_df to {plot_path}"))
    write_csv(plot_df,plot_path)

log_print(sprintf("FINAL SUCCESS for %s",model_id))
log_close()


