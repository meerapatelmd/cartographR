#' Searching mrconso with 2 LIKE phrases
#' @import mySeagull
#' @export
#' 
double_query_mrconso <-
        function(search_word_01, search_word_02, limit_1 = FALSE) {
                                        if (limit_1 == TRUE) {
                                                sql_statement <- paste0("SELECT CUI, STR FROM MRCONSO WHERE STR LIKE '%", search_word_01, "%' AND STR LIKE '%", search_word_02, "%' AND LAT = 'ENG' AND ISPREF = 'Y' 
                                                                LIMIT 1;")
                                                cohort <- mySeagull::get_query("umls", sql_statement)
                                                return(cohort)
                                        } else {
                                                sql_statement <- paste0("SELECT CUI, STR FROM MRCONSO WHERE STR LIKE '%", search_word_01, "%' AND STR LIKE '%", search_word_02, "%' AND LAT = 'ENG' AND ISPREF = 'Y';")
                                                cohort <- mySeagull::get_query("umls", sql_statement)
                                                return(cohort)
                                        }
                                }
