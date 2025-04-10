get_intersections <- function(df, columns, names) {

  intersect_len <- function(set_list) {
    # Need at least two non-empty sets
    
    length(Reduce(intersect(set_list)))
  }
  
  # Convert columns into sets, handling NAs
  sets <- lapply(columns, function(col) {
    lapply(df[!is.na(df[[col]]),col], function(x) strsplit(x, ",")[[1]])
  })
  
  cat(unlist(lapply(sets,length)),"\n")
  
  # Generate all combinations of topics
  topic_combinations <- expand.grid(A = seq_along(sets[[1]]),
                                    B = seq_along(sets[[2]]),
                                    C = seq_along(sets[[3]]))
  cat(length(topic_combinations))
  # Compute Jaccard similarities efficiently
  similarities <- topic_combinations %>%
    rowwise() %>%
    mutate(
      #Jaccard_ABC = jaccard(c(sets[[1]][A], sets[[2]][B], sets[[3]][C])),
      int_AB  = intersect_len(c(sets[[1]][A], sets[[2]][B])),
      int_AC  = intersect_len(c(sets[[1]][A],sets[[3]][C])),
      int_BC  = intersect_len(c(sets[[2]][B], sets[[3]][C]))
    ) %>%
    pivot_longer(cols = starts_with("int_"), names_to = "Comparison", values_to = "Overlap") %>%
    filter(Overlap > 0) %>%
    ungroup() %>%
    mutate(
      Topic_A = ifelse(grepl("A", Comparison), A, NA),
      Topic_B = ifelse(grepl("B", Comparison), B, NA),
      Topic_C = ifelse(grepl("C", Comparison), C, NA)
    )%>%
    select(Topic_A,Topic_B,Topic_C,"Overlap")%>%
    distinct()
  colnames(similarities) = c(names,"Overlap")
  
  return(similarities)
}


jaccard_similarity_matrix <- function(df, columns, names) {
  
  # Jaccard similarity function
  jaccard <- function(set_list) {
    # Need at least two non-empty sets
    
    intersection <- length(Reduce(intersect, set_list))
    union <- length(Reduce(union, set_list))
    
    return(ifelse(union == 0, -1, intersection / union))
  }
  
  # Convert columns into sets, handling NAs
  sets <- lapply(columns, function(col) {
    lapply(df[!is.na(df[[col]]),col], function(x) strsplit(x, ",")[[1]])
  })
  
  cat(unlist(lapply(sets,length)),"\n")
  
  # Generate all combinations of topics
  topic_combinations <- expand.grid(A = seq_along(sets[[1]]),
                                    B = seq_along(sets[[2]]),
                                    C = seq_along(sets[[3]]))
  cat(length(topic_combinations))
  # Compute Jaccard similarities efficiently
  similarities <- topic_combinations %>%
    rowwise() %>%
    mutate(
      #Jaccard_ABC = jaccard(c(sets[[1]][A], sets[[2]][B], sets[[3]][C])),
      Jaccard_AB  = jaccard(c(sets[[1]][A], sets[[2]][B])),
      Jaccard_AC  = jaccard(c(sets[[1]][A],sets[[3]][C])),
      Jaccard_BC  = jaccard(c(sets[[2]][B], sets[[3]][C]))
    ) %>%
    pivot_longer(cols = starts_with("Jaccard_"), names_to = "Comparison", values_to = "Similarity") %>%
    filter(Similarity > 0) %>%
    ungroup() %>%
    mutate(
      Topic_A = ifelse(grepl("A", Comparison), A, NA),
      Topic_B = ifelse(grepl("B", Comparison), B, NA),
      Topic_C = ifelse(grepl("C", Comparison), C, NA)
    )%>%
    select(Topic_A,Topic_B,Topic_C,"Similarity")%>%
    distinct()
  colnames(similarities) = c(names,"Similarity")
  
  return(similarities)
}


get_intersecting_words <- function(df, columns, names) {
  
  intersect_str<-function(set_a,set_b){
    intersect(set_a,set_b)%>%str_flatten_comma()
    
  }
  # Convert columns into sets, handling NAs
  sets <- lapply(columns, function(col) {
    lapply(df[!is.na(df[[col]]),col], function(x) strsplit(x, ",")[[1]])
  })
  
  cat(unlist(lapply(sets,length)),"\n")
  
  # Generate all combinations of topics
  topic_combinations <- expand.grid(A = seq_along(sets[[1]]),
                                    B = seq_along(sets[[2]]),
                                    C = seq_along(sets[[3]]))
  cat(length(topic_combinations))
  # Compute Jaccard similarities efficiently
  similarities <- topic_combinations %>%
    rowwise() %>%
    mutate(
      #Jaccard_ABC = jaccard(c(sets[[1]][A], sets[[2]][B], sets[[3]][C])),
      int_AB  = intersect_str(sets[[1]][A], sets[[2]][B]),
      int_AC  = intersect_str(sets[[1]][A],sets[[3]][C]),
      int_BC  = intersect_str(sets[[2]][B], sets[[3]][C])
    ) %>%
    pivot_longer(cols = starts_with("int_"), names_to = "Comparison", values_to = "Overlap") %>%
    ungroup() %>%
    mutate(
      Topic_A = ifelse(grepl("A", Comparison), A, NA),
      Topic_B = ifelse(grepl("B", Comparison), B, NA),
      Topic_C = ifelse(grepl("C", Comparison), C, NA)
    )%>%
    select(Topic_A,Topic_B,Topic_C,"Overlap")%>%
    distinct()
  colnames(similarities) = c(names,"Overlap")
  
  return(similarities)
}
