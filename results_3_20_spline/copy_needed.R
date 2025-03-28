library(stringr)
library(tidyverse)

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

stm_src_dir<-"results_3_20_spline/stm_objs"
stm_dest_dir<-"results_3_20_spline/using_stms"

dir.create(stm_dest_dir, recursive = TRUE, showWarnings = FALSE)

doc_src_dir<-"results_3_20_spline/prepped_docs"
doc_dest_dir<-"results_3_20_spline/using_docs"
dir.create(doc_dest_dir, recursive = TRUE, showWarnings = FALSE)

ck<-chosen_k|>
  rename(doc_vocab_id=doc_id)|>
  filter(doc_vocab_id %in% chosen)|>
  left_join(meta)|>
  mutate(model_id=paste0(doc_vocab_id,"_",optimal_k,"k"),
         model_src=paste0(stm_src_dir,"/stm_",model_id,".rds"),
         model_dest=paste0(stm_dest_dir,"/stm_",model_id,".rds"),
         doc_src=paste0(doc_src_dir,"/",doc_vocab_id,".rds"),
         doc_dest = paste0(doc_dest_dir,"/",doc_vocab_id,".rds")
         
  )

docs<-ck%>%distinct(doc_src,doc_dest)
objs<-ck%>%distinct(model_src,model_dest)

apply(docs,1,function(a){file.copy(a[1],a[2])})
apply(objs,1,function(a){file.copy(a[1],a[2])})


