
keep_onlyQAQC <- function(data){
  # data is SWMPr data frame
  
  # to test: 
  # make sure datetimestamp, historical, provisionalplus are the only columns that don't start with f_
  # make sure at least one f_ column appears
  
  # to improve:
  # make historical and provisionalplus go away if they're not in the original data frame
  # make the function not work if there aren't any qaqc columns in the data
  
  
  to_keep_qaqc <- c("datetimestamp", "historical", "provisionalplus",
                    grep("^f_", names(data), value = TRUE))
  data[ , names(data) %in% to_keep_qaqc]
}


extract_flag <- function(col){
  # col is a vector of QAQC flags, e.g. a column in a data frame
  flags <- stringr::str_extract(col, "<-?\\d>")
  return(flags)
}


replace_missing_flags <- function(col){
  # col is a vector of QAQC flags, e.g. a column in a data frame
  col[which(col == "" | is.na(col))] <- "<-2>"
  return(col)
}