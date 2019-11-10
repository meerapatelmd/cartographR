#' Makes sure that the coordinate cui and its associated strings match up to the keyword. This applies especially to words like "No" that can mean either "Nitrous Oxide" or "No".
#' @param sql_query_keyword_id id number that identifies the keyword.
#' @param sql_query_keyword keyword to review
#' @return the UMLS MRCONSO STR's that are associated with the coordinate cui
#' @import dplyr
#' @import readr
#' @import typewriteR
#' @export


review_coordinates <-
        function(
                 umls_sql_keyword,
                 path_to_glossary = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/UMLS_MT_MRCONSO/GLOSSARY_UMLS_MT_MRCONSO_QUERY_TERMS.csv") {
                
                glossary <- readr::read_csv(path_to_glossary,
                                           col_types = cols(.default = "c"))
                
                
                coordinates <-
                        glossary %>%
                        dplyr::filter(UMLS_SQL_KEYWORD == umls_sql_keyword) %>% 
                        dplyr::select(UMLS_SQL_KEYWORD_ID, UMLS_SQL_KEYWORD, starts_with("COORDINATE"))

                cat("\t")
                print(coordinates)
                nets <- return_mrconso_net(phrase = umls_sql_keyword,
                                   list_by_cui = TRUE)
                
                coordinate_strs <- nets[[coordinates$COORDINATE_CUI]]
                
                print(coordinate_strs)
                
                typewriteR::tell_me("Would you like to also see the NETS listed by CUI?")
                typewriteR::stop_and_enter()
                
                print(nets)
                
        }
