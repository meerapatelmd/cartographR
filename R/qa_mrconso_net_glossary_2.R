#' This function will deduplicate the GLOSSARY based on the UMLS_QUERY_KEYWORD only
#' @import readr
#' @import somersaulteR
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @import typewriteR
#' @export

qa_mrconso_net_glossary_2 <-
        function(path_to_glossary = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/UMLS_MT_MRCONSO/GLOSSARY_UMLS_MT_MRCONSO_QUERY_TERMS.csv") {
                ##Getting glossary
                glossary_fn <- path_to_glossary
                glossary <- readr::read_csv(glossary_fn, col_types = cols(.default = "c"))
                
                #Getting only the most recent observation per term
                new_glossary <-
                somersaulteR::filter_most_recent_obs_by_group(glossary, 
                                                              group_by_col =  UMLS_SQL_KEYWORD,
                                                              index_date_col = UMLS_SQL_KEYWORD_TIMESTAMP) %>%
                        dplyr::ungroup() %>%
                        somersaulteR::call_mr_clean()
                
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