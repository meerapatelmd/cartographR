#' STR match splitting by parenthesis
#' @import mirroR
#' @import typewriteR
#' @import stringr
#' @import dplyr
#' @export


str_match_bw_parentheses <-
        function(progress_list) {
                ##Outside and within parenthesis
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
                        
                        if (grepl("[(]{1}.*[)]{1}", KEY_CONCEPT_NAME) == TRUE) {
                                KEY_CONCEPT_NAME_A <- stringr::str_remove(KEY_CONCEPT_NAME, "[(]{1}.*")
                                KEY_CONCEPT_NAME_B <- stringr::str_remove(KEY_CONCEPT_NAME, ".*[(]{1}|[)]{1}")
                                KEY_CONCEPT_NAME_B <- stringr::str_remove(KEY_CONCEPT_NAME_B, "[)]")
                                cat("\n")
                                typewriteR::tell_me(Sys.time(), "Concept A:", KEY_CONCEPT_NAME_A)
                                cat("\n\n")
                                cat("\n")
                                typewriteR::tell_me(Sys.time(), "Concept B:", KEY_CONCEPT_NAME_B)
                                cat("\n\n")
                                
                                mrconso_obs <- query_mrconso_data_exact_limit_one(KEY_CONCEPT_NAME_A)
                                if (nrow(mrconso_obs) > 0) {
                                        progress_list$QUEUE$KEY_CUI[1] <- mrconso_obs$CUI
                                        progress_list$QUEUE$KEY_CUI_MRCONSO_STR[1] <- KEY_CONCEPT_NAME_A
                                        progress_list$QUEUE$KEY_TIMESTAMP[1] <- mirroR::get_timestamp()
                                        
                                        progress_list$STAGED <-
                                                dplyr::bind_rows(progress_list$STAGED,
                                                                 progress_list$QUEUE %>%
                                                                         filter(row_number() == 1)
                                                )
                                        
                                        progress_list$QUEUE <- progress_list$QUEUE[-1,]
                                        assign("PROGRESS_LIST", progress_list, envir = globalenv())
                                } else {
                                        mrconso_obs <- query_mrconso_data_exact_limit_one(KEY_CONCEPT_NAME_B)
                                        if (nrow(mrconso_obs) > 0) {
                                                progress_list$QUEUE$KEY_CUI[1] <- mrconso_obs$CUI
                                                progress_list$QUEUE$KEY_CUI_MRCONSO_STR[1] <- KEY_CONCEPT_NAME_B
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
