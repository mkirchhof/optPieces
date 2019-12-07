# standard_csvGermany - reads data from a german formatted .csv file
#                       (sep = ";", dec = ".")
standard_csvGermany <- function(path){
  if(gsub(".*\\.([[:alpha:]]+)$", "\\1", path) != "csv")
    stop("wrong type")
  
  dat <- read.csv2(path)
  
  if(any(dim(dat) == 0))
    stop("empty file")
  
  return(dat)
}