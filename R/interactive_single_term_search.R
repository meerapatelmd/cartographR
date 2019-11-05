#' Performs a single phrase search in MRCONSO rowwise in progress_list$QUEUE
#' @import dplyr
#' @import mirroR
#' @import typewriteR
#' @export

interactive_single_term_search <-
        function(progress_list) {
                for (i in 1:nrow(progress_list$QUEUE)) {
                        KEY_CONCEPT_NAME <- progress_list$QUEUE$KEY_CONCEPT_NAME[1]
                        cat("\n\n")
                        typewriteR::tell_me(Sys.time(), "Starting row", i, "of", total_obs)
                        cat("\n")
                        typewriteR::tell_me(Sys.time(), "Concept:", KEY_CONCEPT_NAME)
                        cat("\n\n")
                        
                        print(progress_list$QUEUE[1,])
                        
                        x <- readline("What is the new search term? `S` to skip. ")
                        if (!(x %in% c("S", "s", ""))) {
                                first_match <- get_first_possible_cui(x)
                                if (nrow(first_match) == 1) {
                                        progress_list$QUEUE$KEY_CUI[1] <- first_match$CUI
                                        progress_list$QUEUE$KEY_CUI_MRCONSO_STR[1] <- first_match$STR
                                        progress_list$QUEUE$KEY_TIMESTAMP[1] <- mirroR::get_timestamp()
                                        
                                        progress_list$STAGED <-
                                                dplyr::bind_rows(progress_list$STAGED,
                                                                 progress_list$QUEUE %>%
                                                                         filter(row_number() == 1)
                                                )
                                        
                                        progress_list$QUEUE <- progress_list$QUEUE[-1,]
                                        assign("PROGRESS_LIST", progress_list, envir = globalenv())
                                } else {
                                        progress_list$STAGED <-
                                                dplyr::bind_rows(progress_list$STAGED,
                                                                 progress_list$QUEUE %>%
                                                                         filter(row_number() == 1)
                                                )
                                        
                                        progress_list$QUEUE <- progress_list$QUEUE[-1,]
                                        assign("PROGRESS_LIST", progress_list, envir = globalenv())
                                }
                        } else {
                                progress_list$STAGED <-
                                        dplyr::bind_rows(progress_list$STAGED,
                                                         progress_list$QUEUE %>%
                                                                 filter(row_number() == 1)
                                        )
                                
                                progress_list$QUEUE <- progress_list$QUEUE[-1,]
                                assign("PROGRESS_LIST", progress_list, envir = globalenv())
                        }
                }
        }
