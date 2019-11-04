#' Sets the key location to iterate on when making updates
#' @param path path to either googlesheet in the format googlesheet_name.tabname or full file path on local
#' @param is.gsheet boolean. TRUE if KEY is a Google Sheet.
#' @importFrom mirCat list_add
#' @export

set_paths <-
        function(new_catalogue_path = NULL, new_key_path = NULL) {
                setClass("cartograph", representation(source = "character", age = "numeric"), 
                         prototype(name = NA_character_, age = NA_real_))
                if (!file.exists("CARTOGRAPH.RData")) {
                        CARTOGRAPH <- list()
                        if (is.null(new_catalogue_path)) {
                                CARTOGRAPH[[1]] <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE"
                        } else {
                                CARTOGRAPH[[1]] <- new_catalogue_path
                        }
                        
                        if (is.null(new_key_path)) {
                                CARTOGRAPH[[2]] <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/KEY"
                        } else {
                                CARTOGRAPH[[2]] <- new_key_path
                        }
                        names(CARTOGRAPH) <- c("CATALOGUE_PATH", "KEY_PATH")
                        mirroR::save_robj(CARTOGRAPH)
                        assign("CARTOGRAPH", CARTOGRAPH, envir = globalenv())
                } else {
                        CARTOGRAPH <- readRDS("CARTOGRAPH.RData")
                        if (!is.null(new_catalogue_path)) {
                                CARTOGRAPH[["CATALOGUE_PATH"]] <- new_catalogue_path
                                mirroR::save_robj(CARTOGRAPH)
                                assign("CARTOGRAPH", CARTOGRAPH, envir = globalenv())
                        }
                        
                        if (!is.null(new_key_path)) {
                                CARTOGRAPH[["KEY_PATH"]] <- new_key_path
                                mirroR::save_robj(CARTOGRAPH)
                                assign("CARTOGRAPH", CARTOGRAPH, envir = globalenv())
                        }
                        
                }
        }