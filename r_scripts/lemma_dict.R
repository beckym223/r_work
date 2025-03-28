library(readr)
library(SnowballC)
library(koRpus.lang.en)
library(koRpus)
library(textstem)
set.kRp.env(
  TT.cmd="manual",
  lang="en",
  TT.options=list(
    path="TreeTagger",
    preset="en"
  )
)

#split = "page"
#split = "paragraph"
split='doc'
csv_path = sprintf("data/raw_csv/split_by_%s.csv",split)

data<- read_csv(csv_path,show_col_types=FALSE)%>%
  filter(grepl(".*[A-Za-z]+.*",text))
  
lemma_dict_path = "data/metadata/lemma_dict.rds"
if (!file.exists(lemma_dict_path)){
  tagged.results <- treetag(data$text, format="obj",lang='en',stemmer = SnowballC::wordStem)
  lemma_dict <- tagged.results@tokens %>%
  dplyr::filter(!lemma %in% c("<unknown>", "@card@") & nchar(token) > 1 & token != tolower(lemma)) %>%
  mutate_all(.,tolower)%>%
  dplyr::distinct(token,.keep_all=T) %>%
  dplyr::arrange(token) %>%
  dplyr::rename(lemma = lemma) %>%
  select(token,lemma,stem)
  saveRDS(lemma_dict,file=lemma_dict_path) 
}else{
  lemma_dict<-readRDS(lemma_dict_path)
}

lemmatized_from_dict = lemmatize_strings(data$text,lemma_dict)

stemmed<-stem_strings(data$text)


data<-data%>%
  mutate(lemmatized = lemmatized_from_dict,
                stemmed=stemmed,
         text_id=doc_id
         )
write_csv(data,sprintf("data/processed_csv/%s_processed.csv",split))

