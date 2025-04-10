library(stringr)
library(tidyverse)
library(dplyr)
library(stm)
library(viridisLite)
library(grid)
library(gridGraphics)
library(gridExtra)
library(png)
source("~/stm_work/r_scripts/plotUtils.R")

meta <- read_csv("results_3_20_spline/chosen_k_with_meta.csv",show_col_types=F)|>
  select(!optimal_k)
chosen_k<- read_csv("results_3_20_spline/chosen_ks.csv",show_col_types=F)
chosen = c(
  "page_none_1gram_10lt",
  "page_none_2gram_10lt",
  "page_none_2gram_1lt",
  "page_lemma_2gram_1lt",
  "page_stem_2gram_1lt",
  "paragraph_stem_1gram_10lt",
  "paragraph_lemma_2gram_1lt",
  "paragraph_none_1gram_1lt",
  "doc_none_1gram_10lt",
  "doc_lemma_1gram_1lt",
  "doc_stem_1gram_1lt"
)
ck<-chosen_k|>
  rename(doc_vocab_id=doc_id)|>
  filter(doc_vocab_id %in% chosen)|>
  left_join(meta)|>
  mutate(model_id=paste0(doc_vocab_id,"_",optimal_k,"k"),
         k_group = as.factor(ifelse(between(optimal_k,8,13),
                                    'around10',
                                    ifelse(between(optimal_k,18,22),
                                           "around20",
                                           'other_k'
                                    )
         )
         )
  )
make_plots<-function(model_id,
                     plot_folder,
                     existing_models=NULL,
                     max_words=10,
                     cloud_only=F,
                     corr_only=F,
                     overwrite_cloud = F,
                     overwrite_corr = F,
                     model_path_format = "results_3_20_spline/using_stms/stm_%s.rds",
                     cloud_file_format = "wc_%s.png",
                     corr_file_format = "corr_%s.png"
){
  require(stm)
  require(dplyr)
  if (is_null(existing_models)){
    existing_models = list()
  }
  cloud.path = file.path(plot_folder,sprintf(cloud_file_format,model_id))
  corr.path = file.path(plot_folder,sprintf(corr_file_format,model_id))
  
  
  existing_paths = existing_models[[model_id]]
  existing_models[[model_id]]= c(cloud.path,corr.path)
  if (!is_null(existing_paths)){
    file.copy(existing_paths[1],cloud.path)
    file.copy(existing_paths[2],corr.path)
    return(existing_models)
  }
  do_cloud = (!file.exists(cloud.path)|overwrite_cloud)&(!corr_only)
  do_corr = (!file.exists(cloud.path)|overwrite_corr)&(!cloud_only)
  
  if (!(do_cloud|do_corr)){
    return(existing_models)
  }
  model_path<-sprintf(model_path_format,model_id)
  model<-read_rds(model_path)
  .k = model$settings$dim$K
  topic_colors<-gg_color_hue(.k)
  
  if(do_cloud){
    make_save_clouds(model,.k,cloud.path,max.words = max_words,topic_colors=topic_colors)
    cat(paste("Saved cloud for",model_id),"\n")
  }
  
  if (do_corr){
    make_save_corr(model,.k,
                   
                   topic_colors,
                   corr.path,
                   900,900,300,
                   connected_only=T,
                   vertex.label.cex = 0.75,
                   vertex.size=20,
                   margin=0
    )
    
    cat(paste("Saved corr for",model_id),"\n")
    
  }
  return(existing_models)
  
}


by_doc_id<-split(ck ,f = ck$doc_vocab_id)
by_doc_id<-by_doc_id[lapply(by_doc_id,nrow)>1]|>
  lapply(function(x){
    mutate(x,grouping=doc_vocab_id)})

by_ppid<-split(ck,f = ck|>select(unit,trunc,ngram))
by_ppid<-by_ppid[lapply(by_ppid,nrow)>1]|>
  lapply(function(x){
    mutate(x,grouping=paste(unit,trunc,ngram,sep="_"))
  })


by_trunc_ngram<-split(ck,f = ck|>select(trunc,ngram))
by_trunc_ngram<-by_trunc_ngram[lapply(by_trunc_ngram,nrow)>1]|>
  lapply(function(x){
    mutate(x,grouping=paste(trunc,ngram,sep="_"))
  })

by_unit<-split(ck,f=ck$unit)|>
  lapply(function(x){
    mutate(x,grouping=unit)
    
  })

splits<-list(doc_id=by_doc_id,
             trunc_ngram = by_trunc_ngram,
             unit = by_unit,
             ppid = by_ppid)
max_words = 10
for (i in seq_along(names(splits))){
  print(names(splits)[i])
}
out_folder = "results_3_20_spline/grouped_more_results"

existing_models=list()
overwrite_cloud=T
overwrite_corr=T


for (idx in seq_along(splits)){
  if (idx==1){
    existing_models=list()
  }
  subgroup_id=names(splits)[idx]
  subgroup = splits[[idx]]
  
  for (i in seq_along(subgroup)){
    df = subgroup[[i]]
    name = names(subgroup[i])
    plot.folder<-file.path(out_folder,subgroup_id,name)
    dir.create(plot.folder,showWarnings = F,recursive=T)
    for (j in seq(nrow(df))){
      
      row = df[j,]
      model_id=row$model_id
      .k = row$optimal_k
      existing_models<-make_plots(model_id,plot.folder,
                                  existing_models = existing_models,
                                  overwrite_cloud = overwrite_cloud,
                                  overwrite_corr=overwrite_corr)
      
  }
  

  }
}

    
    
    
    
    


