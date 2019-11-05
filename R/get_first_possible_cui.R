#' Gets first possible cui for the input phrase in English and ISPREF = 'Y'
#' @import mySeagull
#' @export

get_first_possible_cui <-
        function(search_word) {
                
                                        sql_statement <- paste0("SELECT CUI, STR FROM MRCONSO WHERE STR = '", search_word, "' 
                                                                AND LAT = 'ENG' AND ISPREF = 'Y' LIMIT 1;")
                                        cohort <- mySeagull::get_query("umls", sql_statement)
                                        return(cohort)
                                }
