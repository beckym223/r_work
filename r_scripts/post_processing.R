library(stringr)
library(tidyverse)
library(stm)
library(reshape2)
library(data.table)
library(tibble)
library(logger)
library(splus2R)
source("r_scripts/formats.R")
source('r_scripts/make_md_csv.R')

args<-commandArgs(trailingOnly = T)
output_folder<-args[1]
if (is.na(output_folder)){
  output_folder<-"results"
}
result_folder<-file.path(output_folder,"stm_objs")

md_path<-file.path(output_folder,'results_metadata.csv')
md<-make_results_csv(
  result_folder,
  md_path
)
read_csvs<-function(p){read_csv(p,show_col_types=F)}


plot_df_dir<-get_plot_data_dir(output_folder)

plot_df <- list.files(path=plot_df_dir, full.names = TRUE) %>% 
  lapply(read_csvs) %>% 
  bind_rows()%>%
  inner_join(md,by = join_by(model_id,doc_vocab_id))

write_csv(plot_df,file.path(output_folder,"big_plot_df.csv"))



topic_df_dir<-get_topic_data_dir(output_folder)
topic_df <- list.files(path=topic_df_dir, full.names = TRUE) %>% 
  lapply(read_csvs) %>% 
  bind_rows()%>%
  inner_join(md,by = join_by(model_id,doc_vocab_id))

write_csv(topic_df,file.path(output_folder,"big_topic_df.csv"))

topic_range = topic_df$k%>%range()
minmax<-topic_df%>%
  reshape2::melt(id.vars=c('unit','model_id','topic'),measure.vars = c('semcoh','exclus'),variable.name='measure')%>%
  group_by(model_id,unit,measure)%>%
  summarise(mean=mean(value),
            .groups='keep'
            )%>%
  group_by(measure,unit)%>%
  summarise(min=min(mean),
            max=max(mean),
            .groups='keep'
            )


by_doc_id<-split(topic_df , f = topic_df$doc_vocab_id)
i=1
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
      geom_text(data = metrics%>%filter(measure=='exclus'),aes(label=label),hjust=1, nudge_y=0.02) +
      geom_text(data = metrics%>%filter(measure!='exclus'),aes(label=label),hjust=0, nudge_y=0.3)+
      facet_wrap(~measure_name,nrow=2,scales='free')+
      scale_x_continuous(breaks = seq(topic_range[1],topic_range[2],5),minor_breaks = seq(topic_range[1],topic_range[2]))+
      labs(title=doc_id,
           x = "Number of Topics (K)",
           y = "Mean Value over Topics",
           color = "Metric") +
      theme_grey()
    #show(p)
plot_folder<-file.path(output_folder,'k_plots')
dir.create(plot_folder, recursive = TRUE, showWarnings = FALSE)

plot_path=file.path(plot_folder,paste0(doc_id,".png"))         
ggsave(plot_path,width=8,height=5,units='in')
}
