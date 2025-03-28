library(stringr)
library(tidyverse)
library(stm)
library(reshape2)
library(data.table)
library(tibble)
library(logger)
library(splus2R)

args<-commandArgs(trailingOnly = T)
output_folder<-args[1]

if (is.na(output_folder)){
  output_folder<-"~/stm_work/results_3_20_spline"
}
result_folder<-file.path(output_folder,"stm_objs")

md_path<-file.path(output_folder,'results_metadata.csv')
md<-make_results_csv(
  result_folder,
  md_path
)
read_csvs<-function(p){read_csv(p,show_col_types=F)}


topic_df_dir<-get_topic_data_dir(output_folder)
topic_df <- list.files(path=topic_df_dir, full.names = TRUE) %>% 
  lapply(read_csvs) %>% 
  bind_rows()%>%
  inner_join(md,by = join_by(model_id,doc_vocab_id))

topic_df<-read_csv(file.path(output_folder,"big_topic_df.csv"),show_col_types=F)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

label_traces<-function(names){
  col<-gg_color_hue(length(names))
  scale_color_manual(labels = names,values=col)
  
}


sd_low<-function(.data){
  mean(.data,na.rm=T)-sd(.data)
}

sd_high<-function(.data){
  mean(.data,na.rm=T)+sd(.data)
}

by_doc_id<-split(topic_df , f = topic_df$doc_vocab_id)
i=1
old_results<-results
results<-c()
for (i in seq_along(by_doc_id)){
  data<-by_doc_id[[i]]
  sub_minmax<-minmax%>%filter(unit==data$unit[[1]])%>%
    mutate(measure_name = ifelse(measure=='exclus',"Exclusivity","Semantic Coherence"))
  doc_id<-data$doc_vocab_id[[1]]
  data_m<-data%>%
    reshape2::melt(id.vars=c('k'),measure.vars = c('exclus','semcoh'),variable.name='measure')
  metrics<-data_m%>%
    group_by(k,measure)%>%
    summarise(value_mean=mean(value),
              .groups='keep')%>%
    group_by(measure)%>%
    mutate(peak = peaks(value_mean,3),
           delta = c(0,diff(value_mean)),
           d = delta>diff(range(value_mean))/diff(range(k)),
           measure_name = ifelse(measure=='exclus',"Exclusivity","Semantic Coherence"),
           color_index = abs(value_mean - mean(value_mean)),
           color_index2 = abs((value_mean - mean(value_mean))/sd(value_mean))
    ) %>%
    group_by(k)%>%
    mutate(possible = any(peak&d),
           label = ifelse(possible,as.character(k),"")
    )
  p<-ggplot(metrics,aes(x=k,y=value_mean))+
    geom_smooth(linewidth=0.8,color='pink',fill='pink')+
    geom_line()+geom_point()+
    geom_blank(data=sub_minmax,aes(y=min,x=50))+
    #geom_text(data = metrics%>%filter(measure=='exclus'),aes(label=label),hjust=1, nudge_y=0.02) +
    #geom_text(data = metrics%>%filter(measure!='exclus'),aes(label=label),hjust=0, nudge_y=0.3)+
    facet_wrap(~measure_name,nrow=2,scales='free')+
    scale_x_continuous(breaks = seq(topic_range[1],topic_range[2],5),minor_breaks = seq(topic_range[1],topic_range[2]))+
    labs(
      #title=doc_id,
         x = "Number of Topics (K)",
         y = "Mean Value over Topics",
         color = "Metric") +
    theme_grey()
  show(p)


  tryCatch({
    #s<-as.integer(readline("Good k: "))
    new_t = tibble(doc_id=doc_id,val = readline("Good k: "))%>%
      separate_longer_delim(val,regex(" ?, ?"))%>%
      mutate(optimal_k = as.integer(val))%>%
      select(!val)
      
  },
    warning=function(e){stop(e)}
  )
  results<-c(results,new_t)
}
result_df<-tibble(doc_vocab_id=names(results),k = as.double(results))
optimal_topic_df<-topic_df%>%inner_join(result_df)

write_csv(optimal_topic_df,file.path(output_folder,"optimal_topic_df.csv"))

