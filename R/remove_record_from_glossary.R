#' Completely removes all traces of a query search
#' @param sql_query_keyword_id id number that identifies the keyword. If NULL, all records with parameter sql_query_keyword will be filtered out instead.
#' @param sql_query_keyword keyword to remove
#' @return An updated glossary that is written at its existing file destination
#' @import dplyr
#' @import readr
#' @import typewriteR
#' @export


remove_record_from_glossary <-
        function(umls_sql_keyword_id = NULL, 
                 umls_sql_keyword,
                 path_to_glossary = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/UMLS_MT_MRCONSO/GLOSSARY_UMLS_MT_MRCONSO_QUERY_TERMS.csv") {
                
                glossary <- readr::read_csv(path_to_glossary,
                                           col_types = cols(.default = "c"))
                
                if (!is.null(umls_sql_keyword_id)) {
                        remove_from_glossary_df <-
                                glossary %>%
                                dplyr::filter(UMLS_SQL_KEYWORD_ID == umls_sql_keyword_id,
                                              UMLS_SQL_KEYWORD == umls_sql_keyword)
                } else {
                        remove_from_glossary_df <-
                                glossary %>%
                                dplyr::filter(UMLS_SQL_KEYWORD == umls_sql_keyword)
                }

                
                new_glossary <-
                        dplyr::setdiff(glossary, remove_from_glossary_df)
                
                typewriteR::tell_me("There are", nrow(new_glossary), "rows in the updated glossary while there are", nrow(glossary), "in the current key. Overwrite?")
                typewriteR::stop_and_enter()
                
                readr::write_csv(new_glossary, path = path_to_glossary)
        }
