library(stringr)
library(tidyverse)
library(stm)
library(reshape2)
library(data.table)
library(tibble)
library(logger)
library(splus2R)
source("r_scripts/formats.R")
output_folder="~/stm_work/results_3_20_spline"
topic_df<-read_csv(file.path(output_folder,"big_topic_df.csv"),show_col_types = F)

result_df<-read_csv("results_3_20_spline/chosen_ks.csv",show_col_types=F)%>%
    rename(
        k=optimal_k,
        doc_vocab_id=doc_id
    )%>%
    left_join(topic_df)%>%
    select(model_id,doc_vocab_id,unit,trunc,ngram,lt,k)%>%
    distinct()%>%
    mutate(is_2gram=as.integer(ngram==2),
           filter_freq = as.integer(lt==10)
           )%>%
    filter(k<50)
model<-lm(k~unit+trunc+is_2gram+filter_freq,data=result_df)
plot(model)

summary(model)
keep<-"doc_stem_1gram_1lt"
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
gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}
label_traces<-function(names){
    col<-gg_color_hue(length(names))
    scale_color_manual(labels = names,values=c(col))}
result_df<-read_csv("results_3_20_spline/chosen_ks.csv",show_col_types=F)%>%
    mutate(chosen=TRUE)

chosen<-c("page_lemma_2gram_10lt",
          "page_none_2gram_10lt", #(double)
          "paragraph_lemma_1gram_1lt")
chosen_topic<-topic_df%>%
    filter(doc_vocab_id %in% chosen)
by_doc_id<-split(chosen_topic , f = chosen_topic$doc_vocab_id)
i=1
results<-list()

for (i in seq_along(by_doc_id)){
    data<-by_doc_id[[i]]
    sub_minmax<-minmax%>%filter(unit==data$unit[[1]])%>%
        mutate(measure_name = ifelse(measure=='exclus',"Exclusivity","Semantic Coherence"))
    doc_id<-data$doc_vocab_id[[1]]
    print(doc_id)
    data_m<-data%>%
        reshape2::melt(id.vars=c('k'),measure.vars = c('exclus','semcoh'),variable.name='measure')
    metrics<-data_m%>%
        group_by(k,measure)%>%
        summarise(value_mean=mean(value),
                  .groups='keep')%>%
        group_by(measure)%>%
        mutate(
            doc_id=doc_id,
            peak = peaks(value_mean,3),
               delta = c(0,diff(value_mean)),
               d = delta>diff(range(value_mean))/diff(range(k)),
               measure_name = ifelse(measure=='exclus',"Exclusivity","Semantic Coherence"),
               color_index = abs(value_mean - mean(value_mean)),
               color_index2 = abs((value_mean - mean(value_mean))/sd(value_mean))
        ) %>%
        group_by(k)%>%
        mutate(possible = any(peak&d),
               label = ifelse(possible,as.character(k),"")
        )%>%
        left_join(result_df,by=join_by(doc_id,k==optimal_k))%>%
        filter(k<=60)
    
    not_chosen<-metrics%>%
        filter(is.na(chosen))
    chosen<-metrics%>%
        filter(chosen)
    ggplot(metrics,aes(x=k,y=value_mean))+
        #geom_smooth(linewidth=0.8,color='pink',fill='pink')+
        geom_line()+
        geom_point(data=not_chosen,size=0.7)+
        geom_point(data=chosen,aes(color='blue'))+
        geom_blank(data=sub_minmax,aes(y=min,x=50))+
        #label_traces(c(TRUE,FALSE))+
        scale_color_manual(labels = c("Potential K"),values=c('red'))+
        #geom_text(data = metrics%>%filter(measure=='exclus'),aes(label=label),hjust=1, nudge_y=0.02) +
        #geom_text(data = metrics%>%filter(measure!='exclus'),aes(label=label),hjust=0, nudge_y=0.3)+
        facet_wrap(~measure_name,nrow=2,scales='free')+
        scale_x_continuous(breaks = seq(topic_range[1],topic_range[2],5),
                           #minor_breaks = seq(topic_range[1],topic_range[2])
                           )+
        labs(
            #title=doc_id,
             x = "Number of Topics (K)",
             y = "Mean Value over Topics",
             color = ""
        ) +
        theme_grey()
    show(p)
    

}