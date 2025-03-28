library(stringr)
library(tidyverse)
library(dplyr)
library(stm)
library(reshape2)
library(ggplot2)
library(GGally)
library(tibble)
library(car)
library(viridisLite)
library(zoo)
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
i=1
for (i in seq_along(splits)){
    df= splits[[i]]
    name = names(splits[i])
    print(name)
    j=1
    for (j in seq(nrow(df))){
        row = df[j,]
        model_id=row$model_id
        doc_id=row$doc_vocab_id
        .k=row$optimal_k
        model_path<-paste0("results_3_20_spline/stm_objs/stm_",model_id,".rds")
        model<-read_rds(model_path)
        processed<-read_rds(paste0("results_3_20_spline/prepped_docs/",doc_id,".rds"))$processed
        data<-processed$meta
        
        no_num_labels<-get_labels(model,4,2,F)
        
        plot_df = get_plot_df(model,data)%>%
            mutate(no_num_labels= as.factor(no_num_labels[topic]))
        facet_labeller<-as_labeller(function(factor){no_num_labels[as.integer(factor)]})
        topic_colors<-gg_color_hue(.k)
        topic_labels<-get_labels(model,4,2)
        scale_fill = scale_fill_manual(labels=topic_labels,
                          values=topic_colors)
        scale_color<-scale_color_manual(labels=topic_labels,
                               values=topic_colors)
        theta_df<-get_prop_prevalence(model,processed)%>%
            mutate(Topic=topic,
                   topic=as.integer(topic)
                   )
        area<-ggplot(plot_df,aes(x=year,y=y,
                              #color=label,
                              fill=Topic,
                              #color=Topic,
                              #label=label
        )
        )+
            geom_area(alpha=1)+
            scale_fill+
            coord_cartesian( xlim = NULL, ylim = c(0,1),expand = F, default = FALSE, clip = 'on')+
            ylab('Estimated Topic Prevalence')+
            labs(
                title=row$model_id
            )+
            #theme_minimal()+
            scale_x_continuous(n.breaks=8)+
            theme(legend.key.spacing.y = unit(0.2,'cm'))+
            guides(fill = guide_legend(byrow = TRUE,ncol = ceiling(.k/18)
            ))
        
        show(area)
        ggplot(plot_df,aes(x=year,
                           y=y,
                                 #color=label,
                                 fill=Topic,
                                 color=Topic
                        
                                 #color=Topic,
                                 #label=label
        )
        )+
            
            #geom_ribbon(aes(ymax=upper_ci,ymin=lower_ci),alpha=0.4,linetype=2)+
            geom_col(data = theta_df,aes(y=value,x=year,fill=Topic))+
            geom_line(aes(linetype='a'),color='black',alpha=1,linewidth=0.3)+
        
            facet_wrap(~Topic,ncol=3,scales='free_y',labeller =facet_labeller, axes='all_x')+
            scale_fill+
            scale_color+
            scale_linetype_manual(values=c(1),labels=c("Estimated effect\nregression spline"))+
            #coord_cartesian( xlim = NULL, ylim = c(0,1),expand = F, default = FALSE, clip = 'on')+
            
            ylab('Estimated Topic Prevalence')+
            labs(
                linetype="",
                title=row$model_id
            )+
            #theme_minimal()+
            scale_x_continuous(breaks = seq(1890,2020,30))+
            theme(legend.key.spacing.y = unit(0.2,'cm'))+
            guides(fill = guide_legend(byrow = TRUE,ncol = ceiling(.k/18)
            ))
        
        png("test_corr.png",width=4,height=4,units='in',res=400)
        plot(topicCorr(model),vlabels=seq(.k),vertex.color = topic_colors,vertex.size=20,text)
        dev.off()
        
    }
    print("")
}


for (i in seq_along(ck$doc_vocab_id)){
    row = ck[i,]
    doc_id=row$doc_vocab_id
    .k=row$optimal_k
    if (!between(.k,17,34)){
        next
    }
    model_path<-paste0("results_3_20_spline/stm_objs/stm_",doc_id,"_",.k,"k.rds")
    model<-read_rds(model_path)
    processed<-read_rds(paste0("results_3_20_spline/prepped_docs/",doc_id,".rds"))$processed
    data<-processed$meta
    plot_df = get_plot_df(model,data)
    p<-ggplot(plot_df,aes(x=year,y=y,
                          #color=label,
                          fill=Topic,
                          #color=Topic,
                          #label=label
    )
    )+
        geom_area(alpha=1)+
        scale_fill_manual(labels=get_labels(model,4,2),
                          values=gg_color_hue(.k))+
        coord_cartesian( xlim = NULL, ylim = c(0,1),expand = F, default = FALSE, clip = 'on')+
        labs(shape=get_labels(model,4,2))+
        ylab('Estimated Topic Prevalence')+
        labs(
            title=row$model_id
        )+
        #theme_minimal()+
        scale_x_continuous(n.breaks=8)+
        theme(legend.key.spacing.y = unit(0.2,'cm'))+
        guides(fill = guide_legend(byrow = TRUE,ncol = ceiling(.k/18)
                                   ))
    
    show(p)
}
