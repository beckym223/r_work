results <- readRDS("~/R_Work/results/results_params_a053f88d.rds")

results = results$results

tp <-results$tokens_processed

dv = docvars(tp)

stm = results$dfm2stm

docs = stm$documents
vocab = stm$vocab
data = stm$meta
plotRemoved(docs, lower.thresh = seq(1, 100, by = 10))

set.seed(02138)
ks<-c(5,10,20,40,80) 
kresult <- searchK(docs, vocab, ks, prevalence=~s(year), data=data,cores=4)
kresults = kresult$results %>% as.numeric()
ggplot(data = kresults, aes(x = as.numeric(K))) +
  # Main series (points)
  geom_point(aes(y = exclus), color = "blue") +
  geom_line(aes(y = exclus), color = "blue") +  # Connect points
  
  # Second series (normalized for dual axis)
  geom_point(aes(y = semcoh / max(semcoh) * max(exclus)), color = "red") +
  geom_line(aes(y = semcoh / max(semcoh) * max(exclus)), color = "red") +
  
  # Custom axis labels
  labs(
    title = "Topic Model SearchK Results",
    x = "Number of Topics (K)",
    y = "Exclusivity",
    color = "Legend"
  ) +
  
  # Second y-axis scaling
  scale_y_continuous(
    sec.axis = sec_axis(~ . / max(exclus) * max(semcoh), name = "Semantic Coherence")
  ) +
  
  theme_minimal()
