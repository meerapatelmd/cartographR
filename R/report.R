
#' Creates a REPORT that contains TABLE_OF_CONTENTS sourced from catalogue, the RAW_IDENTITY, and the MAPPED, PARKED_FOR_DENOVO, and PARKED_FOR_EXTERNAL, and QUEUE statuses
#' @import readr
#' @export

report <-
        function(path_to_identity_csv,
                 path_to_key_csv,
                 filter_out_blank_concepts = TRUE) {
                
                progress_list <- progress_list(path_to_identity_csv = path_to_identity_csv,
                                               path_to_key_csv = path_to_key_csv,
                                               filter_out_blank_concepts = filter_out_blank_concepts)
                
                RAW_IDENTITY <- readr::read_csv(path_to_identity_csv, col_types = cols(.default = "c"))
                
                TABLE_OF_CONTENTS <- readr::read_csv("/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/CATALOGUE_TABLE_OF_CONTENTS_REPORT.csv", col_types = cols(.default = "c"))
                
                REPORT <- list(TABLE_OF_CONTENTS, RAW_IDENTITY, progress_list$MAPPED, progress_list$PARKED_FOR_DENOVO, progress_list$PARKED_FOR_EXTERNAL_FOLLOWUP, progress_list$QUEUE)
                names(REPORT) <- c("TABLE_OF_CONTENTS", "IDENTITY", "MAPPED", "PARKED_FOR_DENOVO", "PARKED_FOR_EXTERNAL_FOLLOWUP", "QUEUE")
                
                return(REPORT)
                
        }