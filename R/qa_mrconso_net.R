#' This function will make sure that the GLOSSARY and list of files in NETS/ match. 
#' @import readr
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @import typewriteR
#' @export

qa_mrconso_net_glossary <-
        function(path_to_glossary = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/UMLS_MT_MRCONSO/GLOSSARY_UMLS_MT_MRCONSO_QUERY_TERMS.csv") {
                ##Getting glossary
                glossary_fn <- path_to_glossary
                glossary <- readr::read_csv(glossary_fn, col_types = cols(.default = "c"))
                
                ##Getting files in NETS/
                files_list <- list.files("/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/UMLS_MT_MRCONSO/NETS", full.names = TRUE)
                
                
                ##Getting {ID}_{QUERY_TERM} combination from glossary to search in files_list
                glossary_keyword_names <-
                        glossary %>%
                        dplyr::transmute(UMLS_SQL_KEYWORD_NAME = paste0(UMLS_SQL_KEYWORD_ID, "_", UMLS_SQL_KEYWORD)) %>%
                        unlist() %>%
                        unname()
                
                ##Getting new cleaned up glossary by making a copy of glossary to new_glossary and filtering out observations not in files_list
                new_glossary <- 
                        glossary %>%
                        dplyr::mutate(UMLS_SQL_KEYWORD_NAME = paste0(UMLS_SQL_KEYWORD_ID, "_", UMLS_SQL_KEYWORD)) %>%
                        dplyr::mutate(IS_IN_FILES_LIST = "")
                       
                for (i in 1:nrow(new_glossary)) {
                        new_glossary$IS_IN_FILES_LIST[i] <- any(grepl(glossary_keyword_names[i], files_list))
                }
                
                new_glossary <- new_glossary %>% 
                                        dplyr::filter(IS_IN_FILES_LIST == TRUE) %>%
                                        dplyr::select(-IS_IN_FILES_LIST, -UMLS_SQL_KEYWORD_NAME)
                
                ##Getting the row count difference between the new qa'd glossary and the current glossary
                typewriteR::tell_me("QA'd glossary has", nrow(new_glossary), "while current glossary has", nrow(glossary), ".", "Would you like to ovewrite?")
                cat("\n")
                typewriteR::stop_and_enter()
                cat("\n")
                
                ##Writing new glossary
                readr::write_csv(new_glossary, path = glossary_fn)
                
                ##Confirming overwrite
                cat("\n")
                typewriteR::tell_me("Glossary successfully updated.")
                cat("\n")
        }