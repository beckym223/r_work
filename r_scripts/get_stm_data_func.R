
get_stm_data<-function(mod,
                       docs,
                       meta,
                       p,
                       s,
                       doc_id,
                       model_id
){
  require(stm)
  require(data.table)
  require(tidyverse)
  require(stringr)
  require(reshape2)
  .k =mod$settings$dim$K
  plot_df<-tibble()
  topic_df<-tibble()
  
  for (i in 1:.k){
    df<-tibble(x=p$x,
                   y=p$means[[i]],
                   topic = as.integer(i)
    )%>%mutate(
      lower_ci=y-p$ci[[i]][1,],
      upper_ci=y+p$ci[[i]][2,]
    )
    
    slope<-as_tibble(s$tables[[i]])[2,]%>%
      rename(
        covar.coef = Estimate,
        covar.sd = `Std. Error`
      )%>%
      mutate(
        topic = as.factor(i),
        sig_a05 = `Pr(>|t|)`<0.05,
        sig_a01 = `Pr(>|t|)`<0.01
      )%>%
      select(topic,!c(`Pr(>|t|)`,`t value`))
    
    plot_df<-bind_rows(plot_df,df)%>%
      distinct()
    
    topic_df<-bind_rows(topic_df,slope)%>%
      distinct()

    
  }
  
  desc<-make.dt(mod,meta)%>%
    select(!c(docnum,text_id))%>%
    melt(id.vars=c('meeting_num'),variable.name = "topic")%>%
    mutate(topic = str_extract(topic,"\\d+"))%>%
    group_by(topic)%>%
    summarise(
      prev.mean= mean(value),
      prev.sd = sd(value,na.rm=T),
      prev.iqr = IQR(value),
      .groups = 'keep')
  
  topic_df<-topic_df%>%
    mutate(
      exclus = exclusivity(mod),
      semcoh = semanticCoherence(mod,docs),
      model_id = model_id,
    doc_vocab_id = doc_vocab_id)%>%
    merge(desc)%>%
    select(model_id,doc_vocab_id,everything())
  
  plot_df<-plot_df%>%
    mutate(
      model_id = model_id,
      doc_vocab_id = doc_vocab_id
    )%>%
    select(model_id,doc_vocab_id,everything())
  
  rownames(plot_df)<-NULL
  rownames(topic_df)<-NULL
  
  output=list(
    plot_df=plot_df,
    topic_df = topic_df
  )
  return(output)
}
