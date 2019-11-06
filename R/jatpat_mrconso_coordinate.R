#' This function takes a column in a dataframe that contains terms to do an exact string match
#' @param dataframe dataframe that contains a column of search terms
#' @param sql_term_col variable that contains the single string of search term
#' @import dplyr
#' @import readr
#' @import projektoR
#' @import mySeagull
#' @import mirroR
#' @export

jatpat_mrconso_coordinate <-
        function(dataframe, sql_term_col) {
                ##Preparing arugments
                sql_term_col <- enquo(sql_term_col)
                
                ##Getting glossary
                glossary_fn <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/UMLS_MT_MRCONSO/GLOSSARY_UMLS_MT_MRCONSO_QUERY_TERMS.csv"
                glossary <- readr::read_csv(glossary_fn, col_types = cols(.default = "c")) 
                
                ##Filtering glossary for coordinates only
                coordinate_glossary <- glossary %>%
                                        dplyr::filter(UMLS_SQL_KEYWORD_CLASS == "COORDINATE")
                
                ##Preparing dataframe
                dataframe <-
                        dataframe %>%
                        dplyr::select(!!sql_term_col) %>%
                        dplyr::distinct()
                
                colnames(dataframe) <- "UMLS_SQL_KEYWORD"
                ##Anti-joining dataframe to get the values that aren't in the glossary
                new_terms_df <-
                        dataframe %>%
                        dplyr::anti_join(coordinate_glossary, by = "UMLS_SQL_KEYWORD") %>%
                        dplyr::mutate(COORDINATE_CUI = "") %>%
                        dplyr::mutate(COORDINATE_STR = "")
                
                if (nrow(new_terms_df) > 0) {
                        for (i in 1:nrow(new_terms_df)) {
                                ##Updating glossary
                                projektoR::append_csv(glossary_fn,
                                                      dataframe = data.frame(
                                                              UMLS_SQL_KEYWORD_TIMESTAMP = mirroR::get_timestamp(),
                                                              UMLS_SQL_KEYWORD_ID = as.character(max(as.double(glossary$UMLS_SQL_KEYWORD_ID)) + 1),
                                                              UMLS_SQL_KEYWORD = new_terms_df$UMLS_SQL_KEYWORD[i],
                                                              UMLS_SQL_KEYWORD_CLASS = "coordinate"
                                                      ))
                                
                                
                                ##Jatpat
                                mrconso_data <- query_mrconso_data_exact_limit_one(new_terms_df$UMLS_SQL_KEYWORD[i])
                                if (nrow(mrconso_data) == 1) {
                                        new_terms_df$COORDINATE_CUI[i] <- mrconso_data$CUI
                                        new_terms_df$COORDINATE_STR[i] <- mrconso_data$STR
                                } else {
                                        new_terms_df$COORDINATE_CUI[i] <- NA
                                        new_terms_df$COORDINATE_STR[i] <- NA
                                }
                        }
                }
                return(new_terms_df)
        }