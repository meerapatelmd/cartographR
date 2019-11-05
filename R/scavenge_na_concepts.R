#' Scavenges and assigns all NA concepts to the "C0027366" cui
#' @param na_values character vector of source values considered to be NA
#' @import typewriteR
#' @import dplyr
#' @import mirroR
#' @export


scavenge_na_concepts <-
        function(progress_list, na_values = c("not available", "N/A", "n/a", "na")) {
                for (i in 1:nrow(progress_list$QUEUE)) {
                        if (i == 1) {
                                total_obs <- nrow(progress_list$QUEUE)
                        }
                        KEY_CONCEPT_NAME <- progress_list$QUEUE$KEY_CONCEPT_NAME[1]
                        cat("\n\n")
                        typewriteR::tell_me(Sys.time(), "Starting row", i, "of", total_obs)
                        cat("\n")
                        typewriteR::tell_me(Sys.time(), "Concept:", KEY_CONCEPT_NAME)
                        cat("\n\n")
                        
                        if (is.na(KEY_CONCEPT_NAME)) {
                                progress_list$QUEUE$KEY_CUI[1] <- "C0027366"
                                progress_list$QUEUE$KEY_CUI_MRCONSO_STR[1] <- "NA"
                                progress_list$QUEUE$KEY_TIMESTAMP[1] <- mirroR::get_timestamp()
                                
                                progress_list$STAGED <-
                                        dplyr::bind_rows(progress_list$STAGED,
                                                  progress_list$QUEUE %>%
                                                          filter(row_number() == 1)
                                        )
                                
                                progress_list$QUEUE <- progress_list$QUEUE[-1,]
                                
                                assign("PROGRESS_LIST", progress_list, envir = globalenv())
                        } else if (KEY_CONCEPT_NAME %in% na_values) {
                                progress_list$QUEUE$KEY_CUI[1] <- "C0027366"
                                progress_list$QUEUE$KEY_CUI_MRCONSO_STR[1] <- "NA"
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
                        
                }
        }

                
                

