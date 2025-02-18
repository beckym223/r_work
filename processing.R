library(dplyr)
library(quanteda)
library(stm)
library(quanteda.textstats)
library(tidyverse)
create_corpus <- function(
    data=NULL,
    remove_numbers=NULL,
    parent_id_field=NULL,
    doc_id_field=NULL,
    text_field=NULL,
    split_hyphens=NULL,
    get_ngram=NULL,
    min_ngram_count=NULL,
    params = list()
) {
  # Override defaults with values from params list, if provided
  args <- list(
    remove_numbers = remove_numbers,
    parent_id_field = parent_id_field,
    doc_id_field = doc_id_field,
    text_field = text_field,
    split_hyphens = split_hyphens,
    min_ngram_count = min_ngram_count,
    get_ngram = get_ngram
  )
  results = list()

  args[names(params)] <- params  # Update defaults with any provided params
  # Rename the parent_id field 
  data <- data %>% rename(parent_id = !!args$parent_id_field) %>%
             separate(doc_id, c("first", "year", "last"), sep = "-")%>% 
             select(-first, -last)%>%
             mutate(year = as.integer(year))

  duped_rows <- data %>% select(args$doc_id_field) %>% duplicated()
  data <- data[!duped_rows,]
  
  results$documents = data%>%rename(id = !!args$doc_id_field, text = !!args$text_field)%>%select(id,text)
  
  
  
  corpus_data<-data %>%
    select(args$doc_id_field,args$text_field,year)

  # Create a corpus
  corpus <- corpus(
    data,
    docid_field = args$doc_id_field,
    text_field = args$text_field,
    unique_docnames = TRUE
    )
   # result[corpus] = corpus
  
  
  # Tokenize the corpus
  tokens <- tokens(
    corpus,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = args$remove_numbers,
    remove_url = FALSE,
    remove_separators = TRUE,
    split_hyphens = args$split_hyphens,
    split_tags = FALSE,
    include_docvars = TRUE,
    padding = FALSE,
    concatenator = "_"
  )
  # result[tokens] = tokens
  
  
  # Stem tokens and remove stopwords
  tokens_processed <- tokens %>%
    tokens_remove(stopwords("en"), padding = FALSE) %>%
    tokens_wordstem(language = quanteda_options("language_stemmer"))
  
  # Extract collocations
  if (args$get_ngram){
    col <- textstat_collocations(tokens_processed,min_count = args$min_ngram_count, tolower = TRUE)
    results$colocation = col
    results$tokens_processed = tokens_compound(tokens_processed, pattern = col)
  }
  

processed.dfm <-dfm(
    tokens_processed,
    tolower = TRUE
  )
results$processed.dfm <- processed.dfm
metadata = data%>%
  select(year)
dfm2stm <- convert(processed.dfm, to = "stm",docvars = metadata)
results$dfm2stm = dfm2stm
  #model.stm <- stm(dfm2stm$documents, dfm2stm$vocab, K = topic.count, data = dfm2stm$meta, init.type = "Spectral") 
  return(results)
}