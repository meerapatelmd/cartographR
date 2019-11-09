#' Generate next ID number
#' @param id_vector vector of id numbers that is already existing. NA values are ignored.
#' @return character vector of length 1 of the next ID number by random addition of a value between 1 and 9
#' @import readr
#' @importFrom gmp as.bigz
#' @export

generate_next_id_number <-
        function(id_vector, path_to_glossary = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/UMLS_MT_MRCONSO/GLOSSARY_UMLS_MT_MRCONSO_QUERY_TERMS.csv") {
                glossary <- readr::read_csv(path_to_glossary, col_types = cols(.default = "c"))
                as.character(max(gmp::as.bigz(glossary %>% filter(!is.na(UMLS_SQL_KEYWORD_ID)) %>% select(UMLS_SQL_KEYWORD_ID) %>% unlist() %>% unname()), na.rm = TRUE) + (sample(1:9, 1)))
        }