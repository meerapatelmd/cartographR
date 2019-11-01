#' Takes IDENTITY and KEY and creates a PROGRESS LIST based on the status of the concept
#' @param identity output from identity functions
#' @param key output from key functions
#' @import dplyr
#' @export

create_progress_list <-
        function(identity,
                 key) {
                
                MAP_IN_PROGRESS <-
                        dplyr::left_join(identity,
                                  key) %>%
                        dplyr::mutate_all(as.character)
                
                
                PROGRESS_LIST <- list()
                PROGRESS_LIST[[1]] <- MAP_IN_PROGRESS %>%
                                                dplyr::filter(!(is.na(KEY_CUI)))
                
                MAP_IN_PROGRESS <-
                        dplyr::setdiff(MAP_IN_PROGRESS,
                                       PROGRESS_LIST[[1]])
                
                PROGRESS_LIST[[2]] <- MAP_IN_PROGRESS %>%
                        dplyr::filter_at(dplyr::vars(dplyr::contains("DENOVO")), dplyr::any_vars(!(is.na(.))))
                
                MAP_IN_PROGRESS <-
                        dplyr::setdiff(MAP_IN_PROGRESS,
                                       PROGRESS_LIST[[2]])
                
                PROGRESS_LIST[[3]] <-
                        MAP_IN_PROGRESS %>%
                        dplyr::filter_at(dplyr::vars(contains("EXTERNAL_FOLLOWUP")), dplyr::any_vars(!(is.na(.))))
                
                MAP_IN_PROGRESS <-
                        dplyr::setdiff(MAP_IN_PROGRESS,
                                       PROGRESS_LIST[[3]])
                
                PROGRESS_LIST[[4]] <- MAP_IN_PROGRESS %>%
                                                dplyr::arrange(IDENTITY_ID) %>%
                                                dplyr::filter(!is.na(KEY_CONCEPT_NAME))
                names(PROGRESS_LIST) <- c("MAPPED", "PARKED_FOR_DENOVO", "PARKED_FOR_EXTERNAL_FOLLOWUP", "QUEUE")
                
                PROGRESS_LIST[[5]] <- PROGRESS_LIST[[4]]
                PROGRESS_LIST[[5]] <- PROGRESS_LIST[[5]][-(1:nrow(PROGRESS_LIST[[5]])),]
                names(PROGRESS_LIST)[5] <- "STAGED"
                
                PROGRESS_LIST$QUEUE <-
                        PROGRESS_LIST$QUEUE %>%
                        dplyr::arrange(IDENTITY_ID, KEY_FIELD)
                
                return(PROGRESS_LIST)
                
        }