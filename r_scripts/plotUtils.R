gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

get_labels<-function(model,n=4,wrap=2,with_num=T)
{
    require(dplyr)
    require(stm)
    require(stringr)
    tibble(t=seq(model$settings$dim$K))%>%
        bind_cols(labelTopics(model,n=n)$frex%>%
                      as_tibble()
        )%>%
        apply(1,
              function(x){str_flatten_comma(x)%>%
                      str_replace("\\s*(\\d*),",ifelse(with_num,"Topic \\1:",""))%>%
                      str_replace_all(sprintf("(([\\w]+, ?){%d})( )",wrap),
                                      "\\1\n"
                      )})
}

get_top_words<-function()

get_plot_df<-function(model,data){
    require(stm)
    require(dplyr)
    est<-estimateEffect(~s(meeting_num),model,data)
    
    p<-plot(est,'meeting_num',method='continuous',omit.plot=T)
    k=model$settings$dim$K
    plot<-tibble()
    for (i in 1:k){
        df<-tibble(x=p$x,
                   y=p$means[[i]],
                   topic = as.integer(i)
        )%>%mutate(
            lower_ci=y-p$ci[[i]][1,],
            upper_ci=y+p$ci[[i]][2,]
        )
        plot<-bind_rows(plot,df)%>%
            distinct()
    }
    plot%>%
        mutate(Topic = as.factor(topic),
               year = x+1886)
}

get_prop_prevalence<-function(model,processed){
    require(stm)
    require(dplyr)
    require(reshape2)
    data<-processed$meta
    data$lengths<-processed$documents%>%sapply(sum)
    K = model$settings$dim$K
    topics = seq(K)
    proportions<-as_tibble(model$theta)
    colnames(proportions) = topics
    proportions%>%bind_cols(data)%>%
        group_by(meeting_num)%>%
        mutate(prop_of_doc=lengths/sum(lengths))%>%
        summarise(across(topics,function(x){sum(x*prop_of_doc)}))%>%
        reshape2::melt(id.vars="meeting_num")%>%
        mutate(topic = as.factor(variable),
               year=meeting_num+1886)
    
}


    
    
