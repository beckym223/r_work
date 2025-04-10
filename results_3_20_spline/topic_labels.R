library(stringr)
library(tidyverse)
library(dplyr)
library(stm)
library(reshape2)
library(tidyr)
library(igraph)
library(ggsankey)
library(quanteda)


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
  "doc_lemma_2gram_1lt",
  "doc_stem_1gram_1lt"
)
ck<-chosen_k|>
  rename(doc_vocab_id=doc_id)|>
  filter(doc_vocab_id %in% chosen)|>
  left_join(meta)|>
  mutate(model_id=paste0(doc_vocab_id,"_",optimal_k,"k"),
         k_group = as.factor(ifelse(between(optimal_k,8,13),
                                    'around10',
                                    ifelse(between(optimal_k,17,22),
                                           "around20",
                                           'other_k'
                                    )
         )
         )
  )

read_label<-function(model_id){
  model_path<-paste0("results_3_20_spline/using_stms/stm_",model_id,".rds")
  
  tryCatch({
    model<-read_rds(model_path)
    return(get_labels(model,10,11,F))
    },
    error = function(e){
    print(e)})
  return("")
  
}
model_id="doc_lemma_2gram_1lt_17k"
read_label_m<-function(model_id){
  model_path<-paste0("results_3_20_spline/using_stms/stm_",model_id,".rds")
  
  tryCatch({
    model<-read_rds(model_path)
    return(labelTopics(model,n=10)$frex)
  },
  error = function(e){
    print(e)})
  return("")
  
}



labels_m<-(ck$model_id)%>%sapply(read_label_m)

l<-labels[c("doc_lemma_2gram_1lt_17k","page_lemma_2gram_1lt_13k","paragraph_lemma_2gram_1lt_11k")]%>%
  sapply(function(x)corpus(x)%>%tokens(remove_punct=T)%>%dfm()%>%as.matrix())
%>%
  sapply(tokens)

alabels<-(ck$model_id)%>%sapply(read_label)
topic_names = as.data.frame(lapply(labels, `length<-`, max(sapply(labels, length)))) 
write_csv(topic_names,"results_3_20_spline/subset_topic_labels.csv")
!is.na(df[[col1]])
  topic_names[!is.na(topic_names[["page_lemma_2gram_1lt_13k"]]),"page_lemma_2gram_1lt_13k"]
  library(dplyr)
  library(tidyr)
  
  df<-topic_names
  columns = c("doc_lemma_2gram_1lt_17k",
              "page_lemma_2gram_1lt_13k",
              "paragraph_lemma_2gram_1lt_11k")
  names=c("doc",'page','paragraph')
  sets <- lapply(columns, function(col) {
    lapply(df[!is.na(df[[col]]),col], function(x) strsplit(x, ",")[[1]])
  })
  
  topic_combinations <- expand.grid(A = seq_along(sets[[1]]),
                                    B = seq_along(sets[[2]]),
                                    C = seq_along(sets[[3]]))
  
  intersect_str<-function(set_a,set_b){
    int = intersect(set_a,set_b)
    ifelse(is_empty(int),NA,int)
  }
  
  intersect(sets1[[2]],sets2[[10]])
  # Compute Jaccard similarities efficiently
  similarities <- topic_combinations %>%
    rowwise() %>%
    mutate(
      #Jaccard_ABC = jaccard(c(sets[[1]][A], sets[[2]][B], sets[[3]][C])),
      int_AB  = intersect_str(sets[[1]][[A]], sets[[2]][[B]]),
      int_AC  = intersect_str(sets[[1]][[A]], sets[[3]][[C]]),
      int_BC  = intersect_str(sets[[2]][[B]], sets[[3]][[C]])
    )%>%
  
  
    pivot_longer(cols = starts_with("int_"), names_to = "Comparison", values_to = "Overlap")%>%
    mutate(
      Topic_A = ifelse(grepl("A", Comparison), paste0(names[1],A), NA),
      Topic_B = ifelse(grepl("B", Comparison), paste0(names[2],B), NA),
      Topic_C = ifelse(grepl("C", Comparison), paste0(names[3],C), NA)
    )%>%
    select(Topic_A,Topic_B,Topic_C,"Overlap")%>%
      filter(!str_equal('',Overlap))%>%
    distinct()
  colnames(similarities) = c(names,"Overlap")
  
known_words<-similarities%>%
  
  pivot_longer(cols=starts_with("Topic_"),names_to='x')%>%
  
  

set_list = list(sets[[1]][10],sets[[2]][2],sets[[3]][3])
  jaccard <- function(set_list) {
    # Need at least two non-empty sets
    
    intersection <- length(Reduce(intersect, set_list))
    union <- set_list%>%unlist()%>%unique()%>%length()
    return(ifelse(union == 0, -1, intersection / union))
  }

word<-get_intersecting_words(topic_names,
                             c("doc_lemma_2gram_1lt_17k",
                               "page_lemma_2gram_1lt_13k",
                               "paragraph_lemma_2gram_1lt_11k"),
                             c("doc","page","paragraph"))


all_3<-jaccard_similarity_matrix(
  topic_names,
  c("doc_lemma_2gram_1lt_17k",
    "page_lemma_2gram_1lt_13k",
    "paragraph_lemma_2gram_1lt_11k"),
  c("doc","page","paragraph")
  
)
inter<-get_intersections(
  topic_names,
  c("doc_lemma_2gram_1lt_17k",
    "page_lemma_2gram_1lt_13k",
    "paragraph_lemma_2gram_1lt_11k"),
  c("doc","page","paragraph")
  
)



all_inter<-get_intersections(
  topic_names,
  c("doc_lemma_2gram_1lt_17k",
    "page_lemma_2gram_1lt_13k",
    "paragraph_lemma_2gram_1lt_11k"),
  c("doc","page","paragraph")
  
)


doc_page<-all_inter%>%
  select(doc,page,Overlap)%>%
  make_long(doc,page,value=Overlap)%>%
  drop_na(node)


long<-all_inter%>%make_long(doc,page,paragraph,value=Overlap)%>%
  drop_na(node)

long_grouped<-long%>%
  mutate(node_id = paste(x, node, sep = "_"),
         next_node_id = paste(next_x, next_node, sep = "_"))

get_thoughts<-function(model,docs,raw_data,topics...){
  filtered = docs$processed
  texts = tibble(text_id = names(filtered$documents))%>%
    inner_join(raw_data)%>%pull(text)
  
  findThoughts(model,texts,...)
  
  
}


  
doc_stm<-read_rds("results_3_20_spline/using_stms/stm_doc_lemma_2gram_1lt_17k.rds")
page_stm<-read_rds("results_3_20_spline/using_stms/stm_page_lemma_2gram_1lt_13k.rds")
para_stm<-read_rds("results_3_20_spline/using_stms/stm_paragraph_lemma_2gram_1lt_11k.rds")
n=5
split_by_paragraph <- read_csv("data_const/clean_text_dfs/split_by_paragraph.csv",show_col_types=F)
paragraph_docs <- readRDS("results_3_20_spline/using_docs/paragraph_lemma_2gram_1lt.rds")
page_docs <- readRDS("results_3_20_spline/page_docs/paragraph_lemma_2gram_1lt.rds")
filtered = paragraph_docs$processed
para_text = tibble(text_id = names(filtered$documents))%>%
  inner_join(split_by_paragraph)%>%pull(text)
findThoughts(para_stm,para_text,topics=c(5))

model<-read_rds(model_path)

raw_labels = list(labelTopics(doc_stm,n=n),
                  labelTopics(page_stm,n=n),
                  labelTopics(para_stm,n=n))
names = c("doc","page","paragraph")
label_table = tibble(doc = c('fill'),paragraph =c('fill'),page = c('fill'))
for (i in seq(3)){
  name = names[i]
  s = raw_labels[i]%>%as.character()
  label_table[1,name]<-tibble(label = str_extract_all(s,"Topic \\d{1,2}:")[[1]],
            frex = str_extract_all(s,"(?=FREX:).+?\n")[[1]])%>%
    mutate(words = paste(label,
                         str_sub(frex,7)
    ))%>%pull(words)%>%str_flatten(collapse = "\n")
  
}
write_csv(label_table,"results_3_20_spline/label_table.csv")


topics =str_extract_all(s,regex(pattern_str))[[1]]
frex = str_extract_all(s,"\n\\s*FREX:[^\n]+")[[1]]
topics




par(mfrow = c(1, 3),mar=c(0,0,0,0) )

plotQuote(get_labels(model,n=4,wrap=100),width=100,align='left')

png('results_3_20_spline/doc_label.png',width=1000,height=1000,res=200)
plot(doc_stm,main="Document",type='labels',n=4,labeltype = "frex",
     topic.names = sapply(seq(doc_stm$settings$dim$K),
                          
                          function(x)paste0("T",x,": ")))
dev.off()
plot(page_stm,main="Page",labeltype = "frex",xlim=c(0,1),topic.names = sapply(seq(page_stm$settings$dim$K),
                                                                              function(x)paste0("T",x,": ")))
plot(para_stm,main="Paragraph",labeltype = "frex",xlim=c(0,1),topic.names = sapply(seq(para_stm$settings$dim$K),
                                                                                   function(x)paste0("T",x,": ")))


"page_lemma_2gram_1lt_13k",
"paragraph_lemma_2gram_1lt_11k"
write_csv(all_3,"results_3_20_spline/jaccard3.csv",na="")
paragraph_lemma_2gram
sim<-jaccard_similarity_matrix(topic_names,
                               "doc_lemma_2gram_1lt_17k",
                               "page_lemma_2gram_1lt_13k"
                               )


para_page<-jaccard_similarity_matrix(topic_names,
                               "page_lemma_2gram_1lt_13k",
                               "paragraph_lemma_2gram_1lt_11k")

para_doc<-jaccard_similarity_matrix(topic_names,
                               "doc_lemma_2gram_1lt_17k",
                               "paragraph_lemma_2gram_1lt_11k"
)




write_csv(sim,"results_3_20_spline/doc_page_sim.csv")
write_csv(para_page,"results_3_20_spline/page_para_sim.csv")
write_csv(para_doc,"results_3_20_spline/doc_para_sim.csv")

sets1 <- lapply(df[!is.na(df[[col1]]),col1], function(x) strsplit(x, ",")[[1]])
sets2 <- lapply(df[!is.na(df[[col2]]),col2], function(x) strsplit(x, ",")[[1]])
df[[col2]]

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
out_folder = "results_3_20_spline/topic_names"



ggplot(long,aes(x = x, 
                next_x = next_x, 
                node = node, 
                next_node = next_node,
                value=value+10,
                group=interaction(node,next_node),
                fill=interaction(node,x),
                #flow.fill = interaction(node,next_node),
                label=node)
       
) +
  geom_alluvial(
    node.color='black',
    width=0.4,
    #aes(flow.fill=value),
    #flow.fill='grey',
    show.legend=F,
    size=0.2,
    flow.alpha=0.7,na.rm=T) +
  geom_alluvial_text(show.legend=F,size=2) +
  theme_sankey(base_size=12)

