pipe_env<-new.env(parent = .GlobalEnv)
pipe_env$LEMMA_DICT_PATH = "data_pre/lemma_dict.rds"
pipe_env$DEFAULT_DATA_PATH = "data_const/clean_text_dfs/split_by_page.csv"
pipe_env$VOCAB_INFO_FILE = "vocab_removed_info.csv"
pipe_env$NGRAM_Z_THRESHOLD = 10

mkdir<-function(dirname){
    if (!dir.exists(dirname)) {
        dir.create(dirname, recursive = TRUE, showWarnings = FALSE)
    }
}
length(list.files("results_3_19/stm_objs"))
get_plot_data_dir<-function(output_dir){
    file.path(output_dir,"plot_dfs")
    
}

get_plot_data_path<-function(model_id){
    paste0(model_id,'_plot.csv')
}

get_topic_data_dir<-function(output_dir){
    file.path(output_dir,"topic_dfs")
}

get_topic_data_path<-function(model_id){
    paste0(model_id,'_tp.csv')
    
}



get_model_out_path<-function(output_dir,model_id){
    file.path(output_dir, "stm_objs",paste0("stm_", model_id, ".rds"))
}

get_model_save_dir<-function(output_dir){
    file.path(output_dir, "stm_objs")
}

get_pp_id<-function(unit,trunc,ngram){
    sprintf("%s_%s_%sgram",unit,trunc,ngram)
}

get_pp_id_file<-function(pp_id){
    paste0(pp_id,"_dfm.rds")
}

get_doc_vocab_id<-function(pp_id,thresh){
    sprintf("%s_%dlt", pp_id,thresh)
}
get_doc_vocab_id_file<-function(doc_id){
    paste0(doc_id,".rds")
}

get_prepped_docs_path.opt<-function(opt){
    file.path(get_dfm_docs_output_dirs(opt$output_dir)$docs,
              get_doc_vocab_id_file(get_doc_id.opt(opt))
              )
}

get_model_id<-function(doc_id,k){
    return(sprintf("%s_%dk", doc_id, k))
}

get_pp_id.opt<-function(opt){
    get_pp_id(opt$unit,opt$text_truncation,opt$ngram)
}
get_doc_id.opt<-function(opt){
    return(get_doc_vocab_id(get_pp_id.opt(opt),
                     opt$lower_thresh))
}

get_model_id.opt<-function(opt){
    return(get_model_id(get_doc_id.opt(opt),
                 opt$num_topics
    )
    )
}

get_data_path<-function(unit){
    sprintf("data_const/clean_text_dfs/split_by_%s.csv",unit)
}

get_dfm_docs_output_dirs<-function(parent_dir){
    list(dfm = file.path(parent_dir,"dfms"),
         docs = file.path(parent_dir,"prepped_docs")
         )
}
