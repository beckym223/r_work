
library(quanteda)
library(stm)
library(quanteda.textstats)
library(tidyverse)
library(logger)
library(SnowballC)
library(koRpus.lang.en)
library(koRpus)
library(textstem)
source('r_scripts/formats.R')

truncated_to='lemma'
ngram=2
data_path = 'data/processed_csv/doc_processed.csv'
create_corpus <- function(
    data_path,
    truncated_to,
    ngram,
    lemma_dict_path=NULL
     # if it's 0 then we're not getting ngrams
    ) # returns a dfm2stm object
  {
  require(quanteda)
  require(stm)
  require(quanteda.textstats)
  require(tidyverse)
  require(readr)
  require(logger)
  
  if (is_null(lemma_dict_path)&&(truncated_to=='lemma')){
    lemma_dict_path=pipe_env$LEMMA_DICT_PATH
  }
  # Create a corpus
  data<-read_csv(data_path,show_col_type=FALSE)
  #print(colnames(data))

  #print(text_field)
  corpus <- corpus(
    data,
    docid_field = 'text_id',
    text_field = 'text',
    unique_docnames = TRUE
    )
   # result[corpus] = corpus
  
  
  # Tokenize the corpus
  tokens <- quanteda::tokens(
    corpus,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = T,
    remove_url = FALSE,
    remove_separators = TRUE,
    split_hyphens = F,
    split_tags = FALSE,
    include_docvars = TRUE,
    padding = FALSE,
    concatenator = "_",
  )
  tokens_processed <- tokens %>%
    tokens_tolower()%>%
    tokens_remove(stopwords("en")) %>%
    tokens_select(min_nchar=2)
  
  if(ngram>1){
    col <- textstat_collocations(tokens_processed,min_count = 10,size=ngram)%>%
      filter(z>pipe_env$NGRAM_Z_THRESHOLD)
    tokens_processed <-tokens_processed%>%tokens_compound(col)
  }
  # result[tokens] = tokens
  
  #ifelse(truncated_to=="stem",'stemmed',ifelse(truncated_to == "lemma",'lemmatized',
  # Stem tokens and remove stopwords
  
    
if (truncated_to=="lemma"){
  lemma_dict<-readRDS(lemma_dict_path)
  tokens_processed<-tokens_processed%>%
    tokens_replace(pattern=lemma_dict$token,replacement=lemma_dict$lemma,valuetype='fixed')
}
if (truncated_to=="stem"){
  tokens_processed<-tokens_processed%>%
    tokens_wordstem()
}
    
  

metadata = data%>%
  select(text_id,meeting_num)
dfm2stm <- dfm(tokens_processed)%>%
  convert(to = "stm",docvars = metadata)
return(dfm2stm)
}
