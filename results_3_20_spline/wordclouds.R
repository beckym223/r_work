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

splits<-c(by_doc_id,by_trunc_ngram,by_unit,by_ppid)
out_folder = "results_3_20_spline/grouped_results"

existing_models=list()
overwrite_cloud=F
overwrite_corr=F

max_words = 10
for (i in seq_along(splits)){
  df= splits[[i]]
  name = names(splits[i])
  cat(paste("working on",name),"\n")
  plot.out.folder<-file.path(out_folder,name)
  dir.create(plot.out.folder,showWarnings = F,recursive=T)
  for (j in seq(nrow(df))){
    row = df[j,]
    model_id=row$model_id

    cloud.path = file.path(plot.out.folder,paste0(model_id,'_wc.png'))
    corr.path = file.path(plot.out.folder,paste0(model_id,"_corr.png"))
    do_cloud = !file.exists(cloud.path)|overwrite_cloud
    do_corr = !file.exists(cloud.path)|overwrite_corr
    existing_paths = existing_models[[model_id]]
    if (!is_null(existing_paths)){
        file.copy(existing_paths[1],cloud.path)
      
      file.copy(existing_paths[2],corr.path)
      next
    }
    existing_models[model_id] = c(cloud.path,corr.path)
    .k=row$optimal_k
    model_path<-paste0("results_3_20_spline/using_stms/stm_",model_id,".rds")
    model<-read_rds(model_path)
    topic_colors<-gg_color_hue(.k)
    
    
    if (!(do_cloud|do_corr)){
      next
    }
    if(do_cloud){
      plots<-list()
      for (t in seq(.k)){
        topic_col<-topic_colors[t]
        tmp_file <- tempfile(fileext = ".png")
        png(tmp_file, width = 800, height = 600, res = 150)
        
        # Generate word cloud
        cloud(model, topic = t,
              max.words = max_words,
              type = 'model',
              scale = c(4, 1),
              color = topic_col)
        dev.off()

        wordcloud_grob <- grobTree(
          rectGrob(gp = gpar(col = topic_col, fill = NA)), 
          rasterGrob(readPNG(tmp_file), interpolate = TRUE))
        
        title_grob <- grobTree(
          rectGrob(gp = gpar(col = topic_col, fill = NA)),  # Background box
          textGrob(paste("Topic", t), gp = gpar(fontsize = 20, fontface = "bold", col = topic_col))  # Text
        )
        
        plots[[t]] <- arrangeGrob(title_grob, wordcloud_grob, ncol = 1, heights = c(0.1, 0.9))

      }
      
      num_cols <- ceiling(sqrt(.k))  # Approximate square layout
      num_rows <- ceiling(.k / num_cols)
      
      png(cloud.path, width = num_cols * 800, height = num_rows * 600, res = 150)  # Adjust size & resolution
      grid.newpage()
      grid.arrange(grobs = plots, ncol = num_cols, nrow = num_rows)
      dev.off()  # Close PNG device
      cat(paste("Saved cloud for",model_id),"\n")
      
    }
    
    if (do_corr){
      tc<-topicCorr(model)
      
      included<-tc$poscor[seq(.k),]%>%apply(2,function(row) sum(row)>1)
      included.topics = seq(.k)[included]
      
      png(corr.path,units='px',width = 900,900,res=300)
      par(mar=c(0,0,0,0)+.1)
      
      plot(tc,
           topics = included.topics,
           vlabels=included.topics,
           vertex.color = topic_colors[included], 
           vertex.label.cex = 0.75,
           vertex.size=20,
           margin=0,
      )
      dev.off()
      cat(paste("Saved corr for",model_id),"\n")
      
    }
    
  }
}
