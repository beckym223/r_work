
data<-by_doc_id[[i]]
doc_id<-data$doc_vocab_id[[1]]
data_m<-data%>%
    reshape2::melt(id.vars=c('k'),measure.vars = c('exclus','semcoh'),variable.name='measure')
metrics<-data_m%>%
    group_by(k,measure)%>%
    summarise(across(value,list(mean=mean,sd=sd,max=max,min=min,low = function(x){quantile(x,0.25)},high = function(x){quantile(x,0.75)})),
              .groups='keep')%>%
    group_by(measure)%>%
    mutate(peak = peaks(value_mean,3),
           delta = c(0,diff(value_mean)),
           d = delta>diff(range(value_mean))/diff(range(k)),
           measure_name = ifelse(measure=='exclus',"Exclusivity","Semantic Coherence")
    )%>%
    group_by(k)%>%
    mutate(possible = any(peak&d),
           label = ifelse(possible,as.character(k),"")
    )
p<-ggplot(metrics,aes(x=k,y=value_mean))+
    geom_smooth(linewidth=0.8,color='pink',fill='pink')+
    geom_line()+geom_point()+
    geom_text(data = metrics%>%filter(measure=='exclus'),aes(label=label),hjust=1, nudge_y=0.02) +
    geom_text(data = metrics%>%filter(measure!='exclus'),aes(label=label),hjust=0, nudge_y=0.3)+
    facet_wrap(~measure_name,scales='free')+
    scale_x_continuous(breaks = seq(20,50,5))+
    labs(title=doc_id,
         x = "Number of Topics (K)",
         y = "Mean Value over Topics",
         color = "Metric") +
    theme_minimal()
show(p)