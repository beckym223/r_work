object 'hello' not found 
Traceback: 
 [1] "4: value[[3L]](cond)"                                                       
 [2] "3: tryCatchOne(expr, names, parentenv, handlers[[1L]])"                     
 [3] "2: tryCatchList(expr, classes, parentenv, handlers)"                        
 [4] "1: tryCatch({"                                                              
 [5] "       hello"                                                               
 [6] "       doc_vocab_id <- get_doc_id.opt(opt)"                                 
 [7] "       model_id <- get_model_id.opt(opt)"                                   
 [8] "       log_print(springf(\"Working with model %s with slurm job %d\", "     
 [9] "           model_id, opt$id))"                                              
[10] "       plot_dir <- get_plot_data_dir(opt$output_dir)"                       
[11] "       topic_dir <- get_topic_data_dir(opt$output_dir)"                     
[12] "       model_dir <- get_model_save_dir(opt$output_dir)"                     
[13] "       mkdir(plot_dir)"                                                     
[14] "       mkdir(topic_dir)"                                                    
[15] "       mkdir(model_dir)"                                                    
[16] "       model_out_path <- get_model_out_path(opt$output_dir, model_id)"      
[17] "       stm_fitted <- file.exists(model_out_path)"                           
[18] "       plot_path = file.path(plot_dir, get_plot_data_path(model_id))"       
[19] "       topic_path = file.path(topic_dir, get_topic_data_path(model_id))"    
[20] "       stm_processed <- (file.exists(plot_path) && file.exists(topic_path))"
[21] "       train_stm <- (!stm_fitted | (opt$if_exists %in% c(\"ignore\", "      
[22] "           \"retrain_only\")))"                                             
[23] "       process_stm <- (!stm_processed | (opt$if_exists %in% c(\"ignore\", " 
[24] "     ..."                                                                   
