
library(tidyverse)
library(stm)
library(reshape2)
library(data.table)
library(tibble)
library(logger)
library(splus2R)
source("r_scripts/formats.R")
source('r_scripts/make_md_csv.R')

output_folder<-'results_3_20_spline'


result_folder<-file.path(output_folder,"stm_objs")

md_path<-file.path(output_folder,'results_metadata.csv')
md<-make_results_csv(
    result_folder,
    md_path
)
units=c("doc","page","paragraph")
truncations=c("lemma","stem","none")
ngram=c(1,2)
lower_threshes=c(1,10)
ks<-seq(5,80)

opt_list<-list()
i=1
opt=list()
for(u in seq_along(units)){
    opt$unit<-units[[u]]
    for(t in seq_along(truncations)){
        opt$text_truncation<-truncations[[t]]
        for (n in seq_along(ngram)){
            opt$ngram<-ngram[[n]]
            for (l in seq_along(lower_threshes)){
                opt$lower_thresh<-lower_threshes[l]
                for (k in seq_along(ks)){
                    opt$num_topics<-ks[[k]]
                    opt_list[[i]]<-opt
                    i=i+1
                }
                
            }
        }
    }
}

i=1
data<-tibble()
for (i in seq_along(opt_list)){
    opt<-opt_list[[i]]
    
    model_id<-get_model_id.opt(opt)
    data<-opt%>%
        as_tibble()%>%
        mutate(model_id=model_id,
               doc_vocab_id=get_doc_id.opt(opt),
               missing=!(model_id %in% md$model_id)
        )%>%
        bind_rows(data)
}



still_missing<-data%>%filter(missing)
out.path<-file.path(output_folder,'missing.csv')
write_csv(still_missing,out.path)
