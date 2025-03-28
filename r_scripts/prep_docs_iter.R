#options(warn=-1)
library(optparse)
library(digest)
library(quanteda)
library(stm)
library(dplyr)
library(tidyverse)
library(logger)
source("~/stm_work/r_scripts/formats.R")
source('~/stm_work/r_scripts/get_processed_docs.R')
source("~/stm_work/r_scripts/get_lemma_dict.R")
#source('utils/text_processing.R')
opt_parse_list<-list(make_option(c("-o",'--output_dir'), action = 'store', type = 'character', default = 'pipe_run', 
            help = "Where to save the processed docs"),
            make_option('--redo_lemma',action = 'store_true',default=FALSE)
            )





units=c("doc","page","paragraph")
truncations=c("lemma","stem","none")
ngram=c(1,2)
lower_threshes=c(1,10)
opt<-parse_args(OptionParser(option_list = opt_parse_list))

opt$output_dir<-"results_3_19"
dir.create(opt$output_dir, recursive = TRUE, showWarnings = FALSE)

opt_list<-list()
if (opt$redo_lemma){
  log_info("Redoing lemmatization")
  l<-get_lemma_dict(pipe_env$DEFAULT_DATA_PATH,pipe_env$LEMMA_DICT_PATH,T)
  log_info(sprintf("Saved to %s",pipe_env$LEMMA_DICT_PATH))
}
i<-1
for(u in seq_along(units)){
  opt$unit<-units[[u]]
  for(t in seq_along(truncations)){
    opt$text_truncation<-truncations[[t]]
    for (n in seq_along(ngram)){
      opt$ngram<-ngram[[n]]
      for (l in seq_along(lower_threshes)){
        opt$lower_thresh<-lower_threshes[l]
        opt_list[[i]]<-opt
        i=i+1
      }
    }
  }
}

vocab_df<-data.frame()
i=1
log_info("Iterating through options")
for (i in seq_along(opt_list)){
  opt<-opt_list[[i]]
  
  full_id<-get_doc_id.opt(opt)
  output<-get_processed_docs(opt$unit,
                             opt$text_truncation,
                             opt$ngram,
                             opt$lower_thresh,
                             opt$output_dir
                             )
  dfm<-output$dfm
  processed<-output$processed
  new_row<-tibble(
    id=full_id,
    preprocess_id=get_pp_id.opt(opt),
    unit=opt$unit,
    trunc=opt$text_truncation,
    lthresh=opt$lower_thresh,
    bigram=(opt$ngram==2),
    start_vocab = length(dfm$vocab),
    total_words=length(processed$vocab),
    words_removed=length(processed$words.removed),
    total_docs=length(processed$documents),
    docs_removed=ifelse(is.null(processed$docs.removed),0,processed$docs.removed)
    )
  vocab_df<-bind_rows(vocab_df,new_row)%>%distinct()
  
}
log_info("Saving vocab dataframe")
vocab_csv_path<- file.path(opt$output_dir,pipe_env$VOCAB_INFO_FILE)
log_info("Saving vocab dataframe to {vocab_csv_path}")
write_csv(vocab_df,vocab_csv_path)
