source('~/stm_work/r_scripts/formats.R')
source("~/stm_work/r_scripts/create_corpus.R")
get_processed_docs<-function(unit,
                             truncation,
                             ngram,
                             thresh,
                             data.out.dir
                             ){
  
  
require(quanteda)
require(stm)
require(dplyr)
require(tidyverse)

pp_id<-get_pp_id(unit,truncation,ngram)
dir.out = get_dfm_docs_output_dirs(data.out.dir)

dfm_dir = dir.out$dfm
doc_dir = dir.out$docs
dir.create(dfm_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)


pp_id_path = file.path(dfm_dir,get_pp_id_file(pp_id))
doc_id<-get_doc_vocab_id(pp_id,thresh)
document_save_path<- file.path(doc_dir,get_doc_vocab_id_file(doc_id))


if (file.exists(document_save_path)){
  output<-readRDS(document_save_path)
  if (!is_null(output$processed)&&!is_null(output$dfm)){
    return(output)
  }
}


if (!file.exists(pp_id_path)){
  data_path = get_data_path(unit)
  dfm<-create_corpus(data_path,
                     truncation,
                     ngram
                     )
  
  write_rds(dfm,file=pp_id_path)
}else{
  dfm<-readRDS(pp_id_path)
}
  
processed <- prepDocuments(dfm$documents, dfm$vocab, lower.thresh = thresh,meta=dfm$meta)

output<-list(processed = processed,
             dfm=dfm
             )
write_rds(output,document_save_path)
return(output)
}
