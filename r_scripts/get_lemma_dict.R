library(readr)
library(SnowballC)
library(koRpus.lang.en)
library(koRpus)
library(textstem)
data_path = "data_const/clean_text_dfs/split_by_page.csv"


get_lemma_dict<-function(
        data_path,
        save_path,
        overwrite = F
){
    
    require(readr)
    require(SnowballC)
    require(koRpus.lang.en)
    require(koRpus)
    require(textstem)
    
    
    if (!overwrite & file.exists(save_path)){
        return(readRDS(save_path))
    }
    
    set.kRp.env(
        TT.cmd="manual",
        lang="en",
        TT.options=list(
            path="TreeTagger",
            preset="en"
        )
    )
    
    data<- read_csv(data_path,show_col_types=FALSE)%>%
        filter(grepl(".*[A-Za-z]+.*",text))
    tagged.results <- treetag(data$text, format="obj",lang='en',stemmer = SnowballC::wordStem)
    lemma_dict <- tagged.results@tokens %>%
        dplyr::filter(!lemma %in% c("<unknown>", "@card@") & nchar(token) > 1 & token != tolower(lemma)) %>%
        mutate_all(.,tolower)%>%
        dplyr::distinct(token,.keep_all=T) %>%
        dplyr::arrange(token) %>%
        dplyr::rename(lemma = lemma) %>%
        select(token,lemma,stem)
    
    saveRDS(lemma_dict,file=save_path) 
    return(lemma_dict)
}