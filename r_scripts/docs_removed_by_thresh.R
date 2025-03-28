library(stm)
library(tidyverse)
library(stringr)
library(reshape2)
dfm_path = "data/dfms"
files <- list.files(path=dfm_path, pattern="*.rds", full.names=T, recursive=FALSE)
files_no_folder<-list.files(path=dfm_path, pattern="*.rds", full.names=F, recursive=FALSE)

data<- tibble(path = files,file=files_no_folder) %>%
    separate_wider_regex(file,patterns=c(unit = "[a-z]+", "_",
                                         trunc = "[a-z]+", ".*",
                                         char_ngram = "\\d","gram.*"
    ),
    cols_remove = F
    )%>%
    separate_wider_regex(file,patterns=c(name=".*","_dfm.rds"),
    cols_remove = F)%>%
    mutate(bigram = char_ngram=='2'
    )%>%
    select(!char_ngram)


threshes = seq(0, 40)
df<-tibble()
for (i in seq_along(files)){
    name<-files[[i]]
    #unit<-str_extract(name,".*/([a-z]+)_.*",group=1)
    dfm<-read_rds(name)
    total_docs<-length(dfm$documents)
    tots = plotRemoved(dfm$documents,lower.thresh=total_docs)
    p<-plotRemoved(dfm$documents,lower.thresh = threshes)
    
    result = mapply(FUN = `/`, p, tots, SIMPLIFY = FALSE)%>%
        as.data.frame()%>%
        mutate(path=name)
    result$lower.thresh = threshes
    df<-bind_rows(df,result)
}
joined<-df%>%left_join(data)
sum <- joined %>%
    reshape2::melt(measure.vars = c('ndocs', 'ntokens', 'nwords')) %>%
    mutate(metric = ifelse(variable == "ndocs", "Documents",
                           ifelse(variable == 'ntokens', "Tokens", "Words")),
           value = value * 100
    ) %>%
    group_by(lower.thresh, metric) %>%
    summarise(
        mean = mean(value),
        std = sd(value),
        upper = max(value),
        lower = min(value),
        .groups = 'keep'
    ) %>%
    reshape2::melt(measure.vars = c('mean', 'upper', 'lower')) %>%
    mutate(display_label = ifelse(variable == "mean", "Mean", "Bounds"))  # New variable for legend

ggplot(sum, aes(x = lower.thresh, y = value, color = display_label, linetype = display_label, linewidth = display_label,groups=variable)) +
    geom_line(data = sum %>% filter(variable == "mean")) +  # Ensure mean is plotted separately
    geom_line(data = sum %>% filter(variable %in% c("upper", "lower"))) +  # Plot upper and lower separately
    geom_vline(aes(xintercept = 10, color = "Threshold", linetype = "Threshold", linewidth = "Threshold")) +  # Add vline to legend
    scale_color_manual(values = c("Mean" = "blue", "Bounds" = "red", "Threshold" = "dark green")) +
    scale_linetype_manual(values = c("Mean" = "solid", "Bounds" = "dashed", "Threshold" = "dashed")) +
    scale_linewidth_manual(values = c("Mean" = 1, "Bounds" = 0.6, "Threshold" = 0.5)) +  # Adjust line sizes
    labs(x = "Lower Threshold",
         y = "Percent Removed",
         color = "",
         linetype = "",
         linewidth = "") +
    facet_wrap(~metric)
ggplot(sum,aes(x=lower.thresh))+
    geom_vline(xintercept=10,color='dark green',linetype='dashed',linewidth=0.4)+
    
    #geom_point(data=half_melted,aes(x=lower.thresh,y=percent),alpha=0.1,color='pink')+
    geom_line(aes(y=mean),color='blue',linewidth=1)+
    geom_line(aes(y=upper),linetype='dotted',color='red',linewidth=0.6)+
    geom_line(aes(y=lower),linetype='dotted',color='red',linewidth=0.6)+
    labs(x = "Lower Threshold",
         y = "Percent Removed")+
    facet_wrap(~metric)


half_melted<-joined%>%
    reshape2::melt(measure.vars = c('ndocs','ntokens','nwords'),
    )%>%
    mutate(percent=value*100,
           metric=ifelse(variable=="ndocs","Documents",ifelse(variable=='ntokens',"Tokens",'Words'))
    )%>%
    select(!value)
melted<-half_melted%>% reshape2::melt(
    measure.vars = c('trunc','bigram','unit'),
    variable.name = 'preprocess',
)
    
ggplot(half_melted,aes(x=lower.thresh,y=percent))+
    facet_wrap(~metric)+
    geom_point(alpha=0.05,color='pink')+geom_smooth()+
    labs(
         x = "Lower Threshold",
         y = "Percent Removed",
         color = "Unit")
ggplot(melted,aes(x=lower.thresh,color=value,y=percent))+
    facet_grid(cols=vars(metric),
               rows=vars(preprocess),
               #cols=vars(unit),
               scales='free')+
    geom_smooth()+
    labs(title="Percent Vocabulary",
         x = "Lower Threshold",
         y = "Percent Removed",
         color = "Unit")


ggplot(df,aes(x=lower.thresh,y=nwords*100))+geom_smooth(color='red')+
    labs(title="Percent Vocabulary Removed by Term Frequency Threshold",
         x = "Lower Threshold",
         y = "Percent of Vocab Removed",
         color = "Unit")+
    geom_vline(xintercept=10,color='gold',linetype='dotted',linewidth=1)+
    ylim(0,100)+xlim(0,40)+theme_minimal()


cust_plot_removed <- function(
                dfm,
                threshes = seq(0, 40, by = 5),
                pcts = NULL,
                max_remove=100
){
        dfm<-readRDS("~/stm_work/data/dfms/doc_none_2gram_dfm.rds")
threshes = seq(0, 40)
pcts<-c(0.01,0.1)
max_remove=100

total_docs<-length(dfm$documents)
tots = plotRemoved(dfm$documents,lower.thresh=total_docs)


if (!is.null(pcts)){
        for (i in seq_along(pcts)){
                new_t<-total_docs*i
                if(new_t<=max_remove){
                threshes<-c(threshes,new_t)
                        
                }
                
        }
}
p<-plotRemoved(dfm$documents,lower.thresh = threshes)

result = mapply(FUN = `/`, p, tots, SIMPLIFY = FALSE)
par(mfrow=c(1,3))

plot(p$lower.thresh,result$ndocs*100,'l',
     main='Percent Documents Removed',
     xlab="Threshold",
     ylab='Percent')
plot(p$lower.thresh,result$nwords*100,'l',
     main="Percent Vocab Removed",
     ylab='Percent',
     xlab="Threshold",
   
     
)
plot(p$lower.thresh,result$ntokens*100,'l',
     main='Percent Tokens Removed',
     xlab="Threshold",
     ylab='Pct'
)
}
