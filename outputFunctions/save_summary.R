# Output Function that saves the summary of the found pieces
# Input: x - an object of class pieceSelection containing the optimization results
Zusammenfassung_speichern <- function(x){
  # open a dialog to choose file destination
  types <- c("csv", "xlsx", "txt")
  path <- tclvalue(tkgetSaveFile(
    filetypes = paste0(paste0("{ {", types, " ", getText("exp_files"), "} {.", 
                              types, "} }", collapse = " "), " { {", 
                       getText("exp_allFiles"), "} * }"), 
    defaultextension = ".csv"))
  chosenType <- gsub(".*[.]([^.]*)$", "\\1", path)
  
  if(nchar(path) > 0)
    switch(chosenType,
           "csv" = write.csv(t(x$summary), path, row.names = TRUE),
           "xlsx" = {getPackage("writexl")
             write_xlsx(t(x$summary), path)},
           "txt" = write.table(t(x$summary), path, row.names = TRUE),
           "RData" = save(dat, file = path))
}