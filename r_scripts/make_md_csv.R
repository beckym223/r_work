source('r_scripts/formats.R')
library(stringr)
library(tidyverse)
make_results_csv<-function(result_path,
                           save_path
                           ){
  require(stringr)
  require(tidyverse)
files <- list.files(path=result_path, pattern="*.rds", full.names=T, recursive=FALSE)
files_no_folder<-list.files(path=result_path, pattern="*.rds", full.names=F, recursive=FALSE)

data<- tibble(path = files,file=files_no_folder) %>%
  separate_wider_regex(file,patterns=c("stm_",
                                       unit = "[a-z]+", "_",
                                       trunc = "[a-z]+", ".*",
                                       char_ngram = "\\d","gram.*_",
                                       char_lt = "\\d\\d?", "lt.*_",
                                       char_k="\\d\\d?","k.*"
                                       ),
                       cols_remove = F
                       )%>%
  mutate(k = as.integer(char_k),
         lt = as.integer(char_lt),
         ngram = as.integer(char_ngram)
         )%>%
  select(!c(char_k, char_lt, char_ngram))%>%
  arrange(unit,trunc,ngram,lt,k)

data.list<-data%>%
  rename(lower_thresh=lt,
         text_truncation = trunc,
         num_topics=k
         )%>%
  split(1:nrow(data))


data$doc_vocab_id<-data.list%>%
  lapply(get_doc_id.opt)%>%
  unlist()%>%
  as.character()
data$model_id<-data.list%>%
  lapply(get_model_id.opt)%>%
  unlist()%>%
  as.character()

write_csv(data,save_path)
return(data)
}
