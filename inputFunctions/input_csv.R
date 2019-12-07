# standard_csv - reads data from a .csv file
standard_csv <- function(path){
  if(gsub(".*\\.([[:alpha:]]+)$", "\\1", path) != "csv")
    stop("wrong type")
  
  dat <- read.csv(path)
  
  if(any(dim(dat) == 0))
    stop("empty file")
  
  return(dat)
}