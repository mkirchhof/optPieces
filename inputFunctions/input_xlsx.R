# standard_xlsx - reads data from a normal .xlsx file
standard_xlsx <- function(path){
  getPackage("readxl")
  
  if(gsub(".*\\.([[:alpha:]]+)$", "\\1", path) != "xlsx")
    stop("wrong type")
  
  dat <- read_excel(path)
  
  if(any(dim(dat) == 0))
    stop("empty file")
  
  return(dat)
}