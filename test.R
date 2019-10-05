
DATA <- readRDS("DATA_TO_VALUESET.RData")
test <- get_valueset(DATA, BC_DATA_DICT_PKEY, VARIABLE_FIELD_NAME)
testtokens <- get_tokens_from_valueset(test)
