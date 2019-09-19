cat("\tLoading base data...\n")
TEMP_FNS <- list.files("G:/GitHub/Grand_Central_Station/rdata", pattern = "[.]{1}RData$", full.names = TRUE)
GRAND_CENTRAL_STATION <- list()
for (i in 1:length(TEMP_FNS)) {
  TEMP_FN       <- TEMP_FNS[i]
  TEMP_OBJ_NAME <- gsub("[.]{1}[a-zA-Z]{1,6}", "", basename(TEMP_FN))
  cat(paste0("\t\tLoading ", basename(TEMP_FN), " as ",   TEMP_OBJ_NAME," in the `GRAND_CENTRAL_STATION` R object...\n"))
  GRAND_CENTRAL_STATION[[i]] <- readr::read_rds(TEMP_FN)
  names(GRAND_CENTRAL_STATION)[i] <- TEMP_OBJ_NAME
}

stop_and_enter()

cat("\n\n")

cat("\tSourcing Grand Central Station functions...\n")
TEMP_FNS <- list.files("G:/GitHub/Grand_Central_Station/r_scripts", full.names = TRUE)
for (i in 1:length(TEMP_FNS)) {
            cat(paste0("\t\tSourcing ", basename(TEMP_FNS[i]), "...\n"))
            source(TEMP_FNS[i])
}
end_this_script()

stop_and_enter()


