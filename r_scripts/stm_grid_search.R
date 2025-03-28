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
source('r_scripts/get_processed_docs.R')
source('r_scripts/formats.R')
source('r_scripts/get_stm_data_func.R')

mkdir<-function(dirname){
  if (!dir.exists(dirname)) {
    dir.create(dirname, recursive = TRUE, showWarnings = FALSE)
  }
}


    option_list = list(
      make_option(c('--unit'), action = 'store', type = 'character', default = 'page', 
                  help = "Whether to process documents at page or paragraph level"),
      
      make_option(c("-t", "--text_truncation"), type = "character", default = "lemma",
                  help = "Truncated text unit: 'stem', 'lemma', or 'none'"),
      
      make_option(c("-n", "--ngram"), type = "integer", default = 1,
                  help = "What n for ngrams (1 is no fancy joining)"),
      
      make_option(c("-k", "--num_topics"), type = "integer", default = 20,
                  help = "Number of topics for STM"),
      
      make_option(c("-s", "--seed"), type = "integer", default = 42,
                  help = "Random seed for reproducibility"),
      
      make_option(c("-o", "--output_dir"), type = "character", default = "results/",
                  help = "Directory to save results"),
      
      make_option(c("-l", "--lower_thresh"), type = "integer", default = 1,
                  help = "Lower threshold for prepDocuments"),
      
      make_option(c("--if_exists"), type='character', default = 'ignore',
                  help = "What to do if stm exists at path: one of ['ignore' 'skip' 'retrain_only' 'reprocess_only']"),
      make_option("--id",type="integer",default=-1,
                  help = "Slurm ID for finding output logs"
                  )
      
      #make_option("--log_path",type='character',default="")
      
    )
    
    
  # Parse options
  opt <- parse_args(OptionParser(option_list = option_list))

  doc_vocab_id <- get_doc_id.opt(opt)
  
  
  model_id <- get_model_id.opt(opt)
  
  model_out_path <- get_model_out_path(opt$output_dir,model_id)
  stm_fitted<-file.exists(model_out_path)
  
  
  plot_dir<-get_plot_data_dir(opt$output_dir)
  topic_dir<-get_topic_data_dir(opt$output_dir)
  model_dir<-get_model_save_dir(opt$output_dir)
  
  plot_path = file.path(plot_dir,get_plot_data_path(model_id))
  topic_path = file.path(topic_dir,get_topic_data_path(model_id))
  stm_processed <-(file.exists(plot_path) && file.exists(topic_path))
  
  train_stm<-(!stm_fitted|(opt$if_exists %in% c("ignore",'retrain_only')))
  process_stm<-(!stm_processed|(opt$if_exists %in% c("ignore",'reprocess_only')))
  
  
  log_dir<-file.path(opt$output_dir,"logs")
  mkdir(log_dir)
  log_path<-file.path(log_dir,paste0(model_id,'.log'))
  # Check if STM model already exists
  if (!(train_stm|process_stm)){
    #log_print(sprintf("STM already fitted at %s",model_out_path))
    stop("No training or processing to be done. Skipping")
  }
  
  log_open(log_path,logdir=F,show_notes=F,compact=T)
  #env$set_opt(opt)
  #log_debug(opt)
  tryCatch(
    {
  
  log_print(sprintf("Working with model %s with slurm job %d", model_id,opt$id))

  
  #log_print("Making output folders")
  mkdir(plot_dir)
  mkdir(topic_dir)
  mkdir(model_dir)
  

  

  
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
    

  if (train_stm){
    log_print(glue::glue("Starting STM fitting with {opt$num_topics} topics..."))
    stm_start_time <- Sys.time()
    stm_obj <- stm(docs,vocab, K = opt$num_topics, init.type = "Spectral",
                   prevalence = ~meeting_num, emtol=0.0001,
                   data = meta,
                   verbose = F
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
    }
  else{
    stm_obj<-read_rds(model_out_path)
    log_print(glue::glue("Loaded stm object from {model_out_path}"))
    }
    
  # get output paths for stm data


  if (process_stm){
    log_print("Processing stm")
    

    
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
    }
  },
  
  error = function(cond){
    log_error(cond$call)
    log_error(cond$message)
    
    },
  finally = options(warn = 0)
)



